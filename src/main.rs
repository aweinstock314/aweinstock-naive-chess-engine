use std::fmt::{self, Display};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

#[derive(Copy, Clone, Debug, PartialEq, Eq, EnumIter)]
#[repr(u8)]
enum Color {
    White,
    Black,
}

impl Color {
    fn from_fen(fen: &str) -> Option<Self> {
        match fen {
            "w" => Some(Color::White),
            "b" => Some(Color::Black),
            _ => None,
        }
    }
    fn to_fen(&self) -> String {
        match self {
            Color::White => "w".to_string(),
            Color::Black => "b".to_string(),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, EnumIter)]
#[repr(u8)]
enum PieceKind {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct Piece {
    color: Color,
    kind: PieceKind,
}

const FEN_PIECES: [[char; 6]; 2] = [
    ['P', 'N', 'B', 'R', 'Q', 'K'],
    ['p', 'n', 'b', 'r', 'q', 'k'],
];

impl Piece {
    fn from_fen(fen: char) -> Option<Self> {
        for color in Color::iter() {
            for kind in PieceKind::iter() {
                if FEN_PIECES[color as usize][kind as usize] == fen {
                    return Some(Piece { color, kind });
                }
            }
        }
        None
    }
    fn to_fen(&self) -> String {
        format!("{}", FEN_PIECES[self.color as usize][self.kind as usize])
    }
}

impl Display for Piece {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", FEN_PIECES[self.color as usize][self.kind as usize])
    }
}

#[derive(Copy, Clone, Default)]
struct Bitboard {
    boards: [[u64; 6]; 2],
}

impl fmt::Debug for Bitboard {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.to_fen())
    }
}

impl Bitboard {
    fn get(&self, i: u64, j: u64) -> Option<Piece> {
        for color in Color::iter() {
            for kind in PieceKind::iter() {
                if self.boards[color as usize][kind as usize] & (1 << (8 * i + j)) != 0 {
                    return Some(Piece { color, kind });
                }
            }
        }
        None
    }
    fn set(&mut self, i: u64, j: u64, piece: Option<Piece>) {
        for color in Color::iter() {
            for kind in PieceKind::iter() {
                if Some(Piece { color, kind }) == piece {
                    self.boards[color as usize][kind as usize] |= 1 << (8 * i + j);
                } else {
                    self.boards[color as usize][kind as usize] &= !(1 << (8 * i + j));
                }
            }
        }
    }

    fn from_fen(fen: &str) -> Option<Self> {
        let mut board = Self::default();
        let mut expecting_slash = false;
        let mut x = 0;
        let mut y = 0;
        for c in fen.chars() {
            match c {
                _ if expecting_slash => {
                    if c == '/' {
                        expecting_slash = false;
                    } else {
                        return None;
                    }
                }
                '0'..='8' => {
                    x += c as u64 - '0' as u64;
                }
                _ => {
                    board.set(x, y, Some(Piece::from_fen(c)?));
                    x += 1;
                }
            }
            if x == 8 {
                x = 0;
                y += 1;
                expecting_slash = true;
            } else if x > 8 {
                return None;
            }
        }
        Some(board)
    }

    fn to_fen(&self) -> String {
        let mut result = String::new();
        for y in 0..8 {
            let mut num_spaces = 0;
            for x in 0..8 {
                if let Some(piece) = self.get(x, y) {
                    if num_spaces > 0 {
                        result += &format!("{}", num_spaces);
                        num_spaces = 0;
                    }
                    result += &piece.to_fen();
                } else {
                    num_spaces += 1;
                }
            }
            if num_spaces > 0 {
                result += &format!("{}", num_spaces);
            }
            if y != 7 {
                result += "/";
            }
        }
        result
    }
}

impl Display for Bitboard {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for y in 0..8 {
            for x in 0..8 {
                if let Some(piece) = self.get(x, y) {
                    write!(f, "{}", piece)?;
                } else {
                    write!(f, " ")?;
                }
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}

#[derive(Debug)]
struct CastleFlags(u8);

impl CastleFlags {
    fn from_fen(fen: &str) -> Option<Self> {
        // TODO: validation
        let mut flags = CastleFlags(0);
        if fen.contains("K") {
            flags.0 |= 1 << 0;
        }
        if fen.contains("Q") {
            flags.0 |= 1 << 1;
        }
        if fen.contains("k") {
            flags.0 |= 1 << 2;
        }
        if fen.contains("q") {
            flags.0 |= 1 << 3;
        }
        Some(flags)
    }
    fn to_fen(&self) -> String {
        let mut result = String::new();
        if self.0 & (1 << 0) != 0 {
            result += "K";
        }
        if self.0 & (1 << 1) != 0 {
            result += "Q";
        }
        if self.0 & (1 << 2) != 0 {
            result += "k";
        }
        if self.0 & (1 << 3) != 0 {
            result += "q";
        }
        if result.is_empty() {
            result += "-";
        }
        result
    }
}

const FEN_COLS: [char; 8] = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'];
const FEN_ROWS: [char; 8] = ['8', '7', '6', '5', '4', '3', '2', '1'];

fn parse_row_and_column(fen: &str) -> Option<(u8, u8)> {
    if fen.len() != 2 {
        return None;
    }
    let mut chars = fen.chars();
    let x = chars.next()?;
    let y = chars.next()?;

    let x = FEN_COLS.iter().position(|z| *z == x)? as u8;
    let y = FEN_ROWS.iter().position(|z| *z == y)? as u8;
    Some((x, y))
}

/// Represents an en passant position as 0bzxxxyyy
/// - z is set if the en passant square is active
/// - x and y are 3 bits each
#[derive(Debug)]
struct EnPassant(u8);

impl EnPassant {
    fn from_fen(fen: &str) -> Option<Self> {
        if fen == "-" {
            Some(EnPassant(0))
        } else {
            let mut pos = 1 << 7;
            let (x, y) = parse_row_and_column(fen)?;
            pos |= (x & 7) << 3;
            pos |= y & 7;
            Some(EnPassant(pos))
        }
    }
    fn to_fen(&self) -> String {
        if self.0 & (1 << 7) != 0 {
            let x = (self.0 >> 3) & 7;
            let y = self.0 & 7;
            format!("{}{}", FEN_COLS[x as usize], FEN_ROWS[y as usize])
        } else {
            "-".to_string()
        }
    }
}

#[derive(Debug)]
struct FenRecord {
    board: Bitboard,
    active_color: Color,
    castle_flags: CastleFlags,
    en_passant: EnPassant,
    halfmove_clock: u32,
    fullmove_number: u32,
}

impl FenRecord {
    fn from_fen(fen: &str) -> Option<Self> {
        let parts: Vec<&str> = fen.split_whitespace().collect();
        if parts.len() != 6 {
            return None;
        }
        let board = Bitboard::from_fen(parts[0])?;
        let active_color = Color::from_fen(parts[1])?;
        let castle_flags = CastleFlags::from_fen(parts[2])?;
        let en_passant = EnPassant::from_fen(parts[3])?;
        let halfmove_clock = parts[4].parse().ok()?;
        let fullmove_number = parts[5].parse().ok()?;
        Some(FenRecord {
            board,
            active_color,
            castle_flags,
            en_passant,
            halfmove_clock,
            fullmove_number,
        })
    }
    fn to_fen(&self) -> String {
        let mut result = String::new();
        result += &self.board.to_fen();
        result += " ";
        result += &self.active_color.to_fen();
        result += " ";
        result += &self.castle_flags.to_fen();
        result += " ";
        result += &self.en_passant.to_fen();
        result += " ";
        result += &format!("{} ", self.halfmove_clock);
        result += &format!("{}", self.fullmove_number);
        result
    }
}

fn main() {
    let examples = [
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
        "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1",
        "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2",
        "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2",
    ];
    for example in examples.iter() {
        let record = FenRecord::from_fen(example).unwrap();
        println!("{:?}", record);
        assert_eq!(example, &&record.to_fen());
    }
}
