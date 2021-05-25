use std::fmt::{self, Display};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

#[derive(Copy, Clone, Debug, PartialEq, Eq, EnumIter)]
#[repr(u8)]
enum Color {
    White,
    Black,
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
}

impl Display for Piece {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", FEN_PIECES[self.color as usize][self.kind as usize])
    }
}

#[derive(Copy, Clone, Debug, Default)]
struct Bitboard {
    boards: [[u64; 6]; 2],
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

fn main() {
    let board = Bitboard::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR").unwrap();
    println!("{}", board);
}
