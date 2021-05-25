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
    fn flip(self) -> Self {
        match self {
            Color::White => Color::Black,
            Color::Black => Color::White,
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
        assert!(i <= 8 && j <= 8);
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
                    write!(f, ".")?;
                }
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}

#[derive(Copy, Clone, Debug)]
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
#[derive(Copy, Clone, Debug)]
struct EnPassant(u8);

impl EnPassant {
    fn from_fen(fen: &str) -> Option<Self> {
        if fen == "-" {
            Some(Self::none())
        } else {
            let (x, y) = parse_row_and_column(fen)?;
            Some(Self::from_pos(x, y))
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
    fn from_pos(x: u8, y: u8) -> Self {
        let mut pos = 1 << 7;
        pos |= (x & 7) << 3;
        pos |= y & 7;
        EnPassant(pos)
    }
    fn none() -> Self {
        EnPassant(0)
    }
}

#[derive(Clone, Debug)]
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
    fn try_move(
        &self,
        result: &mut Vec<Self>,
        x0: i8,
        y0: i8,
        x1: i8,
        y1: i8,
        piece: Piece,
        en_passant: EnPassant,
    ) {
        if x1 < 0 || x1 >= 8 || y1 < 0 || y1 >= 8 {
            return;
        }
        let mut newmove = self.clone();
        newmove.board.set(x0 as _, y0 as _, None);
        newmove.active_color = newmove.active_color.flip();
        newmove.en_passant = en_passant;
        newmove.halfmove_clock += 1;
        // The fullmove number increments after black's turn
        if newmove.active_color == Color::White {
            newmove.fullmove_number += 1;
        }
        // TODO: emulate the existence of a pawn at the old en-passant position for the purposes of capture-checking
        if let Some(other_piece) = self.board.get(x1 as _, y1 as _) {
            // can't capture a piece of the same color
            if piece.color == other_piece.color {
                return;
            }
            // pawns can only capture diagonally
            if piece.kind == PieceKind::Pawn && x0 == x1 {
                return;
            }
            // captures reset the halfmove clock
            newmove.halfmove_clock = 0;
        } else if piece.kind == PieceKind::Pawn && x0 != x1 {
            // pawns can only move diagonally if they're capturing
            return;
        }
        // pawn movement resets the halfmove clock
        if piece.kind == PieceKind::Pawn {
            newmove.halfmove_clock = 0;
        }
        let promote_row = match piece.color {
            Color::White => 0,
            Color::Black => 7,
        };
        if piece.kind == PieceKind::Pawn && y1 == promote_row {
            for kind in PieceKind::iter() {
                let mut tmp = newmove.clone();
                tmp.board.set(
                    x1 as _,
                    y1 as _,
                    Some(Piece {
                        color: piece.color,
                        kind,
                    }),
                );
                result.push(tmp);
            }
        } else {
            newmove.board.set(x1 as _, y1 as _, Some(piece));
            result.push(newmove);
        }
    }
    fn move_ray(&self, result: &mut Vec<Self>, x0: i8, y0: i8, dx: i8, dy: i8, piece: Piece) {
        for i in 1..8 {
            let x1 = x0 + i * dx;
            let y1 = y0 + i * dy;
            if x1 < 0 || x1 >= 8 || y1 < 0 || y1 >= 8 {
                break;
            }
            self.try_move(result, x0, y0, x1, y1, piece, EnPassant::none());
            if self.board.get(x1 as _, y1 as _).is_some() {
                break;
            }
        }
    }
    fn moves_for_piece(&self, x: i8, y: i8, piece: Piece) -> Vec<Self> {
        use {Color::*, PieceKind::*};
        let mut result = Vec::new();
        match piece.kind {
            Pawn => {
                let ydir = match piece.color {
                    White => -1,
                    Black => 1,
                };
                let orig_row = match piece.color {
                    White => 6,
                    Black => 1,
                };
                self.try_move(&mut result, x, y, x, y + ydir, piece, EnPassant::none());
                self.try_move(&mut result, x, y, x - 1, y + ydir, piece, EnPassant::none());
                self.try_move(&mut result, x, y, x + 1, y + ydir, piece, EnPassant::none());
                if y == orig_row {
                    self.try_move(
                        &mut result,
                        x,
                        y,
                        x,
                        y + 2 * ydir,
                        piece,
                        EnPassant::from_pos(x as u8, (y + ydir) as u8),
                    );
                }
            }
            Knight => {
                for i in -2i8..=2 {
                    for j in -2i8..=2 {
                        if (i.abs() == 2 && j.abs() == 1) || (i.abs() == 1 && j.abs() == 2) {
                            self.try_move(
                                &mut result,
                                x,
                                y,
                                x + i,
                                y + j,
                                piece,
                                EnPassant::none(),
                            );
                        }
                    }
                }
            }
            Bishop => {
                for i in [-1, 1].iter() {
                    for j in [-1, 1].iter() {
                        self.move_ray(&mut result, x, y, *i, *j, piece);
                    }
                }
            }
            Rook => {
                for i in [-1, 1].iter() {
                    self.move_ray(&mut result, x, y, *i, 0, piece);
                    self.move_ray(&mut result, x, y, 0, *i, piece);
                }
            }
            Queen => {
                for i in -1..=1 {
                    for j in -1..=1 {
                        if i == 0 && j == 0 {
                            continue;
                        }
                        self.move_ray(&mut result, x, y, i, j, piece);
                    }
                }
            }
            King => {
                for i in -1..=1 {
                    for j in -1..=1 {
                        if i == 0 && j == 0 {
                            continue;
                        }
                        // TODO: castling
                        self.try_move(&mut result, x, y, x + i, y + j, piece, EnPassant::none());
                    }
                }
            }
        }
        result
    }
    fn legal_moves(&self) -> Vec<Self> {
        let mut result = Vec::new();
        for x in 0..8 {
            for y in 0..8 {
                if let Some(piece) = self.board.get(x, y) {
                    if piece.color != self.active_color {
                        continue;
                    }
                    result.extend(self.moves_for_piece(x as _, y as _, piece));
                }
            }
        }
        result
    }
}

fn alpha_beta<N, F1, F2, F3>(
    node: &N,
    depth: usize,
    mut alpha: f64,
    mut beta: f64,
    maximize: bool,
    valuation: &F1,
    terminal: &F2,
    children: &F3,
) -> (Option<N>, f64)
where
    F1: Fn(&N) -> f64,
    F2: Fn(&N) -> bool,
    F3: Fn(&N) -> Vec<N>,
{
    if depth == 0 || terminal(node) {
        return (None, valuation(node));
    }
    let (neutral, dot, update, better): (
        f64,
        fn(f64, f64) -> f64,
        &dyn Fn(&mut f64, &mut f64, f64),
        &dyn Fn(f64, f64) -> bool,
    ) = if maximize {
        (
            -std::f64::INFINITY,
            f64::max,
            &|a: &mut f64, _, v| {
                *a = a.max(v);
            },
            &|a, b| a >= b,
        )
    } else {
        (
            std::f64::INFINITY,
            f64::min,
            &|_, b: &mut f64, v| {
                *b = b.min(v);
            },
            &|a, b| a <= b,
        )
    };
    let mut value = neutral;
    let mut best_child = None;
    for child in children(node) {
        let (_, child_value) = alpha_beta(
            &child,
            depth - 1,
            alpha,
            beta,
            !maximize,
            valuation,
            terminal,
            children,
        );
        if better(child_value, value) {
            best_child = Some(child);
        }
        value = dot(value, child_value);
        update(&mut alpha, &mut beta, value);
        if alpha >= beta {
            break;
        }
    }
    (best_child, value)
}

fn main() {
    let mut state = FenRecord::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap();
    for i in 0..100 {
        let (next_state, value) = alpha_beta(&state, 7, -std::f64::INFINITY, std::f64::INFINITY, true, &|node| {
            let mut score = 0.0;
            for kind in PieceKind::iter() {
                let multiplier = match kind {
                    PieceKind::Pawn => 1.0,
                    PieceKind::Knight => 3.0,
                    PieceKind::Bishop => 4.0,
                    PieceKind::Rook => 5.0,
                    PieceKind::Queen => 9.0,
                    PieceKind::King => 1000.0,
                };
                let subboard = node.board.boards[state.active_color as usize][kind as usize];
                score += subboard.count_ones() as f64 * multiplier;
                let subboard = node.board.boards[state.active_color.flip() as usize][kind as usize];
                score -= subboard.count_ones() as f64 * multiplier;
            }
            score
        },
        &|node| {
            node.board.boards[Color::White as usize][PieceKind::King as usize].count_ones() + node.board.boards[Color::Black as usize][PieceKind::King as usize].count_ones() < 2
        },
        &|node: &FenRecord| {
            node.legal_moves()
        });
        //println!("{:?} {:?} {:?}", state, next_state, value);
        if let Some(next_state) = next_state {
            state = next_state;
        } else {
            break;
        }
        println!("Turn {} (value {}): {:?}", i, value, state.to_fen());
        println!("{}", state.board);
    }
}

#[test]
fn test_fen() {
    let examples = [
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
        "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1",
        "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2",
        "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2",
        "8/4k3/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 0",
        "8/4q3/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 0",
        "8/4n3/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 0",
        "8/4r3/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 0",
        "8/4b3/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 0",
    ];
    for example in examples.iter() {
        let record = FenRecord::from_fen(example).unwrap();
        println!("{:?}", record);
        for newmove in record.legal_moves() {
            println!("\t{:?}", newmove.to_fen());
            //println!("{}", newmove.board);
        }
        assert_eq!(example, &&record.to_fen());
    }
}
