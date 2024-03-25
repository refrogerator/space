use std::any::Any;
use std::ascii::AsciiExt;
use std::collections::HashMap;
use std::io::Write;
use std::ops::{Add, AddAssign, SubAssign};
use std::process::{ExitStatus, Stdio};
use std::sync::Arc;

use glow::*;
use sdl2::event::{Event, WindowEvent};
use sdl2::keyboard::Keycode;
use sdl2::keyboard::Mod;
use swash::scale::Source;

use core::str::FromStr;

use syntect::easy::HighlightLines;
use syntect::highlighting::ScopeSelector;
use syntect::highlighting::ScopeSelectors;
use syntect::highlighting::StyleModifier;
use syntect::highlighting::Theme;
use syntect::highlighting::ThemeItem;
use syntect::highlighting::ThemeSettings;
use syntect::highlighting::{Style, ThemeSet};
use syntect::parsing::{SyntaxReference, SyntaxSet};

use notify::{RecommendedWatcher, RecursiveMode, Watcher};

fn create_shader(gl: &glow::Context, vert: &str, frag: &str) -> glow::Program {
    unsafe {
        let program = gl.create_program().unwrap();
        let vs_source = vert;
        let fs_source = frag;
        let vs = gl.create_shader(glow::VERTEX_SHADER).unwrap();
        gl.shader_source(vs, &vs_source);
        gl.compile_shader(vs);
        if !gl.get_shader_compile_status(vs) {
            panic!("{}", gl.get_shader_info_log(vs));
        }
        gl.attach_shader(program, vs);

        let fs = gl.create_shader(glow::FRAGMENT_SHADER).unwrap();
        gl.shader_source(fs, &fs_source);
        gl.compile_shader(fs);
        if !gl.get_shader_compile_status(fs) {
            panic!("{}", gl.get_shader_info_log(fs));
        }
        gl.attach_shader(program, fs);

        gl.link_program(program);
        if !gl.get_program_link_status(program) {
            panic!("{}", gl.get_program_info_log(program));
        }

        gl.detach_shader(program, vs);
        gl.delete_shader(vs);

        gl.detach_shader(program, fs);
        gl.delete_shader(fs);
        return program;
    }
}

#[derive(Clone, Debug)]
struct IVec2 {
    x: i32,
    y: i32,
}

impl IVec2 {
    fn new(x: i32, y: i32) -> Self {
        return IVec2 { x, y };
    }
}

struct Vec2 {
    x: f32,
    y: f32,
}

struct Vec3 {
    x: f32,
    y: f32,
    z: f32,
}

struct Vec4 {
    x: f32,
    y: f32,
    z: f32,
    w: f32,
}

struct Color {
    r: f32,
    g: f32,
    b: f32,
}

impl From<syntect::highlighting::Color> for Color {
    fn from(col: syntect::highlighting::Color) -> Self {
        Self {
            r: col.r as f32 / 255.0,
            g: col.g as f32 / 255.0,
            b: col.b as f32 / 255.0,
        }
    }
}

impl Into<syntect::highlighting::Color> for &Color {
    fn into(self) -> syntect::highlighting::Color {
        syntect::highlighting::Color {
            r: (self.r * 255.0) as u8,
            g: (self.g * 255.0) as u8,
            b: (self.b * 255.0) as u8,
            a: 0,
        }
    }
}

#[derive(Debug)]
struct Anchor {
    top: bool,
    bottom: bool,
    left: bool,
    right: bool,
}

#[derive(Debug)]
enum Offset {
    Auto,
    Px(f32),
    Percent(f32),
    Vw(f32),
    Vh(f32),
    Vmin(f32),
    Vmax(f32),
}

#[derive(Debug)]
struct Layout {
    x: Offset,
    y: Offset,
    w: Offset,
    h: Offset,
    anchor: Anchor,
}

#[derive(Debug)]
struct Rect {
    x: f32,
    y: f32,
    w: f32,
    h: f32,
}

impl Rect {
    fn resolve_offset(&self, offset: &Offset, horiz: bool) -> f32 {
        let pixels = if horiz { self.w } else { self.h };
        let max_horiz = self.w > self.h;
        match offset {
            Offset::Auto => 1.0,
            Offset::Px(px) => px / pixels,
            Offset::Percent(p) => p / 100.0,
            Offset::Vw(w) => {
                if horiz {
                    self.w / self.h * w
                } else {
                    *w
                }
            }
            Offset::Vh(h) => {
                if horiz {
                    *h
                } else {
                    self.h / self.w * h
                }
            }
            _ => 1.0,
        }
    }
}

impl Layout {
    fn to_screen_coords(&self, container: Rect) -> Rect {
        let ratio = container.w / container.h;
        let mut ret = Rect {
            x: 0.0,
            y: 0.0,
            w: 0.0,
            h: 0.0,
        };

        ret.w = container.resolve_offset(&self.w, true);
        ret.h = container.resolve_offset(&self.h, false);

        ret.x = container.resolve_offset(&self.x, true);
        ret.y = container.resolve_offset(&self.y, false);

        if self.anchor.top && self.anchor.bottom {
            ret.y = 0.5 - ret.h / 2.0;
        } else if self.anchor.top {
            ret.y = ret.y;
        } else if self.anchor.bottom {
            ret.y = 1.0 - ret.y - ret.h;
        }
        if self.anchor.left && self.anchor.right {
            ret.x = 0.5 - ret.w / 2.0;
        } else if self.anchor.left {
            ret.x = ret.x;
        } else if self.anchor.right {
            ret.x = 1.0 - ret.x;
        }

        return ret;
    }
}

struct Button {
    layout: Layout,
}

struct Frame {
    layout: Layout,
}

enum UiComponent {
    Frame(Frame),
    Button(Button),
}

struct Glyph {
    tex: NativeTexture,
    bearing: (i32, i32),
    x: i32,
    y: i32,
}

#[derive(PartialEq)]
enum CursorMode {
    Block,
    Bar,
    Half,
    Underline,
}

#[derive(Debug, Clone)]
enum Position {
    Range(IVec2, IVec2),
    Block(IVec2, IVec2),
    Lines(i32, i32),
    Point(IVec2),
    Nothing,
}

enum Argument {}

#[derive(serde::Deserialize)]
struct CSyntaxColors {
    strings: String,
    numbers: String,
    keywords: String,
    types: String,
    macros: String,
    operators: String,
    functions: String,
    methods: String,
    comments: String,
    text: String,
}

#[derive(serde::Deserialize)]
struct CUiColors {
    foreground: String,
    background: String,
    unfocused_text: String,
    current_line: String,
    cursor: String,
}

#[derive(serde::Deserialize)]
struct CColorscheme {
    colors: HashMap<String, String>,
    syntax: CSyntaxColors,
    ui: CUiColors,
}

struct UiColors {
    foreground: Color,
    background: Color,
    unfocused_text: Color,
    current_line: Color,
    cursor: Color,
}

struct Colorscheme {
    ui: UiColors,
    syntax_theme: syntect::highlighting::Theme,
}

impl From<CColorscheme> for Colorscheme {
    fn from(scheme: CColorscheme) -> Self {
        Colorscheme {
            ui: UiColors {
                background: hex_code_to_color(&scheme.colors[&scheme.ui.background]),
                foreground: hex_code_to_color(&scheme.colors[&scheme.ui.foreground]),
                unfocused_text: hex_code_to_color(&scheme.colors[&scheme.ui.unfocused_text]),
                current_line: hex_code_to_color(&scheme.colors[&scheme.ui.current_line]),
                cursor: hex_code_to_color(&scheme.colors[&scheme.ui.cursor]),
            },
            syntax_theme: create_syntax_theme(&scheme),
        }
    }
}

#[derive(PartialEq)]
enum EditorMode {
    Normal,
    Insert,
    OperatorPending(fn(&mut EditorState, Position)),
    ArgumentPending(String, fn(&mut EditorState)),
    SecArgumentPending(String, fn(&mut EditorState)),
}

#[derive(serde::Deserialize)]
struct EditorConfig {
    tab_width: i32,
    relative_line_numbers: bool,
    colorscheme: String,
    text_size: u32,
}

#[derive(Debug, Clone)]
struct LineDiff {
    line: i32,
    old: String,
    new: String,
}

#[derive(Debug, Clone)]
enum EditEvent {
    Delete(Position, String),
    InsertModeEdit(Vec<LineDiff>)
}

#[derive(Debug, Clone)]
struct UndoHistory {
    edits: Vec<EditEvent>,
    pos: usize,
}

impl UndoHistory {
    fn push(&mut self, edit: EditEvent) {
        self.edits.truncate(self.pos);
        self.pos += 1;
        self.edits.push(edit);
    }

    fn pop(&mut self) -> Option<EditEvent> {
        if self.pos > 0 {
            self.pos -= 1;
            Some(self.edits[self.pos].to_owned())
        } else {
            None
        }
    }
    fn get_redo(&mut self) -> Option<EditEvent> {
        if self.pos < self.edits.len() {
            self.pos += 1;
            Some(self.edits[self.pos - 1].to_owned())
        } else {
            None
        }
    }
}

fn get_res_file_path(filename: &str) -> String {
    if cfg!(debug_assertions) {
        return format!("res/{}", filename);
    }

    if !cfg!(target_os = "windows") {
        for path in std::env::var("XDG_DATA_DIRS").unwrap().split(':') {
            let full_str = format!("{}/space/res/{}", path, filename);
            let res = std::path::Path::new(full_str.as_str()).try_exists();
            if let Ok(true) = res {
                return full_str;
            }
        }
        String::new()
    } else {
        "".to_string()
    }
}

struct EditorState<'a, 'b> {
    contents: Vec<String>,
    filename: Option<String>,
    view: IVec2,
    cursor: IVec2,
    mode: EditorMode,
    argument: String,
    command_amount: String,
    drawing_context: &'a DrawingContext<'b>,
    config: EditorConfig,
    colorscheme: Colorscheme,
    syntax: SyntaxReference,
    syntax_set: SyntaxSet,
    undo_history: UndoHistory,
}

impl<'a, 'b> EditorState<'a, 'b> {
    fn load_file(&mut self, _filename: &str) {
        let filename = _filename.replace("~", &std::env::var("HOME").unwrap());
        self.syntax = self.syntax_set.find_syntax_for_file(&filename).unwrap().unwrap_or(self.syntax_set.find_syntax_by_extension("rs").unwrap()).to_owned();
        self.contents = std::fs::read_to_string(&filename)
            .unwrap()
            .lines()
            .map(|a| a.to_owned())
            .collect();
        self.filename = Some(filename);
        self.view = IVec2::new(0, 0);
        self.cursor = IVec2::new(0, 0);
        self.mode = EditorMode::Normal;
    }

    // kind of a hack but works for now
    fn save_file_with_sudo(&self, filename: &str, password: &str) {
        let tempfile = "/tmp/".to_string() + "-spacetemp" + std::env::var("USER").unwrap().as_str();
        let mut file = std::fs::File::create(&tempfile).unwrap();
        for i in self.contents.iter() {
            file.write(i.as_bytes()).unwrap();
            file.write(b"\n").unwrap();
        }

        let mut child = std::process::Command::new("sudo")
            .args(["-S", "cp", &tempfile, &filename])
            .stdin(Stdio::piped())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .expect("failed to spawn process");
        let mut stdin = child.stdin.take().expect("cant open stdin");
        let password2 = password.to_owned();
        std::thread::spawn(move || {
            stdin.write_all(password2.as_bytes()).unwrap();
        });
        if !child.wait().expect("failed to read output").success() {
            println!("incorrect password");
        }
        println!("password written");
    }

    fn save_file_as(&mut self, filename: &str) {
        match std::fs::File::create(filename) {
            Ok(ref mut file) => {
                for i in self.contents.iter() {
                    file.write(i.as_bytes()).unwrap();
                    file.write(b"\n").unwrap();
                }
            }
            Err(e) => {
                match dbg!(e.kind()) {
                    std::io::ErrorKind::PermissionDenied => {
                        dbg!("chuddy");
                        self.argument.clear();
                        self.sec_arg_pending_mode("sudo password: ", |a| { a.save_file_with_sudo(a.filename.clone().unwrap().as_str(), &a.argument) });
                    }
                    _ => {
                    }
                }        
            }
        }
    }

    fn save_file(&mut self) {
        if let Some(ref filename) = self.filename {
            self.save_file_as(filename.clone().as_str());
        } else {
            self.arg_pending_mode("save file as: ", |a| { a.filename = Some(a.argument.clone()); a.save_file_as(a.argument.clone().as_str()); });
        }
    }

    fn load_config(&mut self) {
        let home = if cfg!(target_os = "windows") {
            // windows is *special*
            std::env::var("USERPROFILE").unwrap()
        } else {
            std::env::var("HOME").unwrap()
        };
        println!("{}", home);
        self.config = toml::from_str(
            std::fs::read_to_string(home.add("/.config/space/config.toml"))
                .unwrap()
                .as_str(),
        )
        .unwrap();
        let colorscheme = self.config.colorscheme.clone();
        self.load_colorscheme(&colorscheme);
    }

    fn load_colorscheme(&mut self, colorscheme: &str) {
        let scheme: CColorscheme = toml::from_str(
            std::fs::read_to_string(get_res_file_path(format!("colorschemes/{}.toml", colorscheme).as_str()))
                .ok()
                .expect(&format!("colorscheme \"{}\" does not exist", colorscheme))
                .as_str(),
        )
        .map_err(|a| panic!("{} in {}.toml", a.message(), colorscheme))
        .unwrap();
        self.colorscheme = scheme.into();
    }

    fn arg_pending_mode(&mut self, query_text: &str, callback: fn(&mut EditorState)) {
        self.mode = EditorMode::ArgumentPending(query_text.to_string(), callback);
        self.drawing_context.video.text_input().start();
    }

    fn sec_arg_pending_mode(&mut self, query_text: &str, callback: fn(&mut EditorState)) {
        self.mode = EditorMode::SecArgumentPending(query_text.to_string(), callback);
        self.drawing_context.video.text_input().start();
    }

    fn open_file_mode(&mut self) {
        self.arg_pending_mode("open file: ", |a| a.load_file(a.argument.clone().as_str()));
    }

    fn repeat(&mut self, command: fn(&mut EditorState, IVec2) -> Position) -> Position {
        let amount = self.amount();
        let mut pos = self.cursor.clone();
        for _ in 0..amount {
            let _pos = command(self, pos);
            pos = self.move_part(_pos);
        }
        Position::Range(self.cursor.clone(), pos)
    }

    fn amount(&mut self) -> i32 {
        let amount: i32 = if self.command_amount.is_empty() {
            1
        } else {
            self.command_amount.parse().unwrap()
        };
        self.command_amount.clear();
        amount
    }

    fn clamp_pos(&self, pos: IVec2) -> IVec2 {
        let contents_len = self.contents.len() as i32;
        let new_line = pos.y.clamp(
            0,
            if contents_len == 0 {
                0
            } else {
                contents_len - 1
            },
        );
        let new_line_len = self.contents[new_line as usize].len() as i32;
        IVec2::new(
            pos.x.clamp(
                0,
                if new_line_len == 0 {
                    0
                } else {
                    match self.mode {
                        EditorMode::Insert | EditorMode::OperatorPending(_) => new_line_len,
                        _ => new_line_len - 1,
                    }
                },
            ),
            new_line,
        )
    }

    fn left(&mut self, cursor: IVec2) -> Position {
        let amount = self.amount();
        return Position::Point(IVec2::new(cursor.x - amount, cursor.y));
    }

    fn right(&mut self, cursor: IVec2) -> Position {
        let amount = self.amount();
        return Position::Point(IVec2::new(cursor.x + amount, cursor.y));
    }

    fn up(&mut self, cursor: IVec2) -> Position {
        let amount = self.amount();
        return Position::Point(IVec2::new(cursor.x, cursor.y - amount));
    }

    fn up_whole_line(&mut self, cursor: IVec2) -> Position {
        let amount = self.amount();
        return Position::Lines(cursor.y, cursor.y - amount);
    }

    // i need to fix this syntax somehow
    fn down(&mut self, cursor: IVec2) -> Position {
        let amount = self.amount();
        return Position::Point(IVec2::new(cursor.x, cursor.y + amount));
    }

    fn down_whole_line(&mut self, cursor: IVec2) -> Position {
        let amount = self.amount();
        return Position::Lines(cursor.y, cursor.y + amount);
    }

    fn current_line_m(&mut self, cursor: IVec2) -> Position {
        return Position::Lines(cursor.y, cursor.y);
    }

    fn move_part(&self, pos: Position) -> IVec2 {
        match pos {
            Position::Range(_, b) => b.clone(),
            Position::Block(_, b) => b.clone(),
            Position::Lines(_, b) => IVec2::new(self.cursor.x, b),
            Position::Point(a) => a.clone(),
            Position::Nothing => self.cursor.clone(),
        }
    }

    fn _move(&mut self, pos: Position) {
        match pos {
            Position::Range(_, b) => {
                self.cursor = self.clamp_pos(b);
            }
            Position::Block(_, b) => {
                self.cursor = self.clamp_pos(b);
            }
            Position::Lines(_, b) => {
                self.cursor.y = b.clamp(0, self.contents.len() as i32 - 1);
            }
            Position::Point(a) => {
                self.cursor = self.clamp_pos(a);
            }
            Position::Nothing => {}
        }
    }

    fn view_down(&mut self, amt: i32) {
        if self.view.y + amt >= self.contents.len() as i32 - 1 {
            self.view.y = self.contents.len() as i32 - 1;
            for _ in 0..(self.contents.len() as i32 - 1) - self.cursor.y {
                self.move_down();
            }
        } else {
            for _ in 0..amt {
                self.move_down();
            }
            self.view.y += amt;
        }
    }

    fn view_up(&mut self, amt: i32) {
        if self.view.y - amt < 0 {
            // for _ in 0..self.view.y {
            //     self.move_up();
            // }
            self.view.y = 0;
        } else {
            // for _ in 0..amt {
            //     self.move_up();
            // }
            self.view.y -= amt;
        }
    }

    fn move_left(&mut self) {
        let pos = self.left(self.cursor.clone());
        self._move(pos);
    }
    fn move_up(&mut self) {
        let pos = self.up(self.cursor.clone());
        self._move(pos);
    }
    fn move_down(&mut self) {
        let pos = self.down(self.cursor.clone());
        self._move(pos);
    }
    fn move_right(&mut self) {
        if self.cursor.x < self.contents[self.cursor.y as usize].len() as i32 {
            self.cursor.x += 1;
        }
    }

    fn move_to_line_start(&mut self) {
        self.cursor.x = 0;
    }

    fn line_end(&self, cursor: IVec2) -> Position {
        Position::Range(
            cursor.clone(),
            IVec2::new(self.get_line(cursor.y).len() as i32, cursor.y),
        )
    }

    fn line_start(&self, cursor: IVec2) -> Position {
        Position::Range(cursor.clone(), IVec2::new(0, cursor.y))
    }

    fn first_line(&self, cursor: IVec2) -> Position {
        Position::Lines(cursor.y, 0)
    }

    fn last_line(&self, cursor: IVec2) -> Position {
        Position::Lines(cursor.y, self.contents.len() as i32 - 1)
    }

    fn move_to_line_end(&mut self) {
        let pos = self.line_end(self.cursor.clone());
        self._move(pos);
    }

    fn get_current_line_mut(&mut self) -> &mut String {
        &mut self.contents[self.cursor.y as usize]
    }

    fn get_line(&self, line: i32) -> &str {
        &self.contents[line as usize]
    }

    fn next_word(&self, cursor: IVec2) -> Position {
        let line = self.get_line(cursor.y);
        for (i, ch) in line
            .chars()
            .skip(cursor.x as usize)
            .collect::<Vec<char>>()
            .windows(2)
            .enumerate()
        {
            let cur = ch[0];
            let next = ch[1];
            let idx = i as i32 + cursor.x;
            if cur.is_whitespace() && !next.is_whitespace()
                || cur.is_alphanumeric() && !next.is_alphanumeric() && !next.is_whitespace()
                || next.is_alphanumeric() && !cur.is_alphanumeric() && !cur.is_whitespace()
            {
                return Position::Range(cursor.clone(), IVec2::new(idx + 1, cursor.y));
            }
        }
        Position::Range(cursor.clone(), IVec2::new(line.len() as i32, cursor.y))
    }

    fn previous_word(&mut self, cursor: IVec2) -> Position {
        let line = self.get_line(cursor.y);
        for (i, ch) in line
            .chars()
            .rev()
            .skip(line.len() - cursor.x as usize)
            .collect::<Vec<char>>()
            .windows(2)
            .enumerate()
        {
            let cur = ch[1];
            let next = ch[0];
            let idx = cursor.x - i as i32;
            if cur.is_whitespace() && !next.is_whitespace()
                || cur.is_alphanumeric() && !next.is_alphanumeric() && !next.is_whitespace()
                || next.is_alphanumeric() && !cur.is_alphanumeric() && !cur.is_whitespace()
            {
                return Position::Range(cursor.clone(), IVec2::new(idx - 1, cursor.y));
            }
        }
        Position::Range(cursor.clone(), IVec2::new(0, cursor.y))
    }

    fn move_word(&mut self) {
        let pos = self.next_word(self.cursor.clone());
        self._move(pos);
    }

    fn move_word_backwards(&mut self) {
        let pos = self.previous_word(self.cursor.clone());
        self._move(pos);
    }

    fn newline_at_cursor(&mut self) {
        let new_line = self.contents[self.cursor.y as usize].split_off(self.cursor.x as usize);
        self.contents.insert(self.cursor.y as usize + 1, new_line);
        self.move_down();
        self.move_to_line_start();
    }

    fn open_above(&mut self) {
        self.contents.insert(self.cursor.y as usize, String::new());
        self.move_to_line_start();
        self.insert_mode();
    }

    fn open_below(&mut self) {
        self.contents.insert(self.cursor.y as usize + 1, String::new());
        self.move_down();
        self.move_to_line_start();
        self.insert_mode();
    }

    fn redo(&mut self) {
        if let Some(event) = self.undo_history.get_redo() {
            match event {
                EditEvent::Delete(pos, _) => {
                    self.delete_pos(pos, false);
                }
                _ => {}
            }
        }
    }

    fn undo(&mut self) {
        if let Some(event) = self.undo_history.pop() {
            match event {
                EditEvent::Delete(pos, content) => {
                    let pos = match pos {
                        Position::Range(a, b) | Position::Block(a, b) => {
                            if a.y != b.y {
                                if a.y < b.y {
                                    a
                                } else {
                                    b
                                }
                            } else {
                                IVec2::new(a.x.min(b.x), a.y)
                            }
                        }
                        Position::Point(a) => a,
                        Position::Lines(a, b) => {
                            IVec2::new(0, a.min(b) - 1)
                        }
                        Position::Nothing => {
                            return;
                        }
                    };
                    self.sanitize_insert_at(&content, pos);
                }
                _ => {}
            }
        }
    }

    fn sanitize_insert_at(&mut self, text: &str, pos: IVec2) {
        let lines = text.lines().collect::<Vec<&str>>();
        if lines.len() > 1 {
            for (i, line) in lines.into_iter().enumerate() {
                self.contents.insert((pos.y + 1) as usize + i, line.to_string());
            }
        } else {
            self.contents[pos.y as usize].insert_str(pos.x as usize, text);
        }
    }

    fn paste(&mut self) {
        let text = self.drawing_context.video.clipboard().clipboard_text().unwrap();
        self.sanitize_insert_at(&text, self.cursor.clone());
    }

    fn get_text_at(&self, pos: Position) -> String {
        match pos {
            Position::Range(start, end) => {
                let mut chud = String::new();
                if start.y != end.y {
                    chud.push_str(
                        self.contents[start.y as usize]
                        .chars()
                        .skip(start.x as usize)
                        .collect::<String>().as_str()
                    );
                    let chud2: String = self.contents[end.y as usize]
                        .chars()
                        .take(end.x as usize)
                        .collect();
                    for _ in (start.y + 1)..(end.y) {
                        chud.push('\n');
                        chud.push_str(self.contents[start.y as usize].as_str());
                    }
                    chud.push_str(chud2.as_str());
                } else {
                    let y = start.y;
                    let (start, end) = (start.x.min(end.x), start.x.max(end.x));
                    dbg!((start, end));
                    chud = self.contents[y as usize]
                        .chars()
                        .skip(start as usize)
                        .take((end - start) as usize)
                        .collect::<String>();
                    dbg!(&chud);
                }
                chud
            }
            Position::Lines(a, b) => {
                let (start, end) = (a.min(b) as usize, a.max(b) as usize);
                self.contents[start..=end].iter().fold(String::new(), |a, b| a + b + "\n")
            }
            _ => { String::new() }
        }
    }

    fn copy_pos(&mut self, pos: Position) {
        let copy_str = self.get_text_at(pos);
        self.drawing_context.video.clipboard().set_clipboard_text(&copy_str).unwrap();
    }

    fn copy(&mut self) {
        self.mode = EditorMode::OperatorPending(|a, b| a.copy_pos(b));
    }

    fn insert(&mut self, text: &str) {
        self.contents[self.cursor.y as usize].insert_str(self.cursor.x as usize, text);
        for _ in 0..text.len() { // TODO dont
            self.move_right();
        }
    }

    fn backspace(&mut self) {
        if self.cursor.x > 0 {
            self.contents[self.cursor.y as usize].remove(self.cursor.x as usize - 1);
            self.move_left();
        } else {
            if self.contents[self.cursor.y as usize].len() > 0 {
                let line = self.contents.remove(self.cursor.y as usize);
                self.move_up();
                self.move_to_line_end();
                self.get_current_line_mut().push_str(&line);
            } else {
                self.contents.remove(self.cursor.y as usize);
                self.move_up();
                self.move_to_line_end();
            }
        }
    }

    fn insert_mode(&mut self) {
        self.mode = EditorMode::Insert;
        self.drawing_context.video.text_input().start();
    }

    fn insert_mode_after(&mut self) {
        if self.cursor.x < self.contents[self.cursor.y as usize].len() as i32 {
            self.cursor.x += 1;
        }
        self.insert_mode();
    }

    // fn normalize_pos_ivec(&self, mut pos: IVec2) -> IVec2 {
    //     pos.y = pos.y.min(self.contents.len().checked_sub(1).unwrap_or(0) as i32);
    //     IVec2::new(pos.x.clamp(0, self.get_line(pos.y).len().checked_sub(1).unwrap_or(0) as i32), pos.y)
    // }

    fn normalize_pos(&self, pos: Position) -> Position {
        match pos {
            Position::Range(a, b) => Position::Range(self.clamp_pos(a), self.clamp_pos(b)),
            Position::Block(a, b) => Position::Block(self.clamp_pos(a), self.clamp_pos(b)),
            Position::Lines(a, b) => Position::Lines(
                a.clamp(0, self.contents.len() as i32 - 1),
                b.clamp(0, self.contents.len() as i32 - 1),
            ),
            Position::Point(a) => Position::Point(self.clamp_pos(a)),
            Position::Nothing => Position::Nothing,
        }
    }

    fn normal_mode(&mut self) {
        // if self.get_current_line().len() == 0 {
        //     self.cursor.x = 0;
        // } else if self.cursor.x >= self.get_current_line().len() as i32 {
        //     self.cursor.x = self.get_current_line().len() as i32 - 1;
        // }
        self.mode = EditorMode::Normal;
        self.cursor = self.clamp_pos(self.cursor.clone());
        self.drawing_context.video.text_input().stop();
    }

    fn delete_pos(&mut self, pos: Position, add_undo: bool) {
        let norm_pos = self.normalize_pos(pos);
        if add_undo {
            self.undo_history.push(EditEvent::Delete(norm_pos.clone(), self.get_text_at(norm_pos.clone())));
        }
        self.copy_pos(norm_pos.clone());
        match norm_pos {
            Position::Range(start, end) => {
                println!("{:?}, {:?}", start, end);
                if start.y != end.y {
                    self.contents[start.y as usize] = self.contents[start.y as usize]
                        .chars()
                        .take(start.x as usize)
                        .collect();
                    self.contents[end.y as usize] = self.contents[end.y as usize]
                        .chars()
                        .skip(end.x as usize)
                        .collect();
                    for _ in (start.y + 1)..(end.y) {
                        self.contents.remove(start.y as usize);
                    }
                } else {
                    if start.x < end.x {
                        self.contents[start.y as usize] = self.contents[start.y as usize]
                            .chars()
                            .take(start.x as usize)
                            .chain(self.contents[start.y as usize].chars().skip(end.x as usize))
                            .collect();
                    } else {
                        self.contents[start.y as usize] = self.contents[start.y as usize]
                            .chars()
                            .take(end.x as usize)
                            .chain(
                                self.contents[start.y as usize]
                                    .chars()
                                    .skip(start.x as usize),
                            )
                            .collect();
                    }
                }
            }
            Position::Lines(start, end) => {
                let smaller = start.min(end);
                let bigger = start.max(end);
                for _ in smaller..=bigger {
                    // TODO fix this in the range somehow
                    if self.contents.len() == 1 {
                        self.contents[0].clear();
                        return;
                    }
                    self.contents.remove(smaller as usize);
                }
            }
            _ => {}
        }
    }

    fn delete_mode(&mut self) {
        self.mode = EditorMode::OperatorPending(|a, b| a.delete_pos(b, true));
    }

    fn change_mode(&mut self) {
        self.mode = EditorMode::OperatorPending(|a, b| { a.delete_pos(b, true); a.insert_mode() });
    }
}

#[derive(Clone, Debug, PartialEq)]
struct Key {
    scancode: sdl2::keyboard::Keycode,
    mods: sdl2::keyboard::Mod,
}

fn string_to_keys(s: &str) -> Vec<Key> {
    let mut keys = Vec::new();
    for _keychord in s.split(" ") {
        let ctrl = if _keychord.len() >= 2 && &_keychord[0..2] == "C-" {
            true
        } else {
            false
        };
        let keychord = if ctrl { &_keychord[2..] } else { _keychord };
        // println!("{}, {}", keychord, ctrl);
        let (scancode, shift) = match keychord {
            "A" => (Keycode::A, true),
            "B" => (Keycode::B, true),
            "C" => (Keycode::C, true),
            "D" => (Keycode::D, true),
            "E" => (Keycode::E, true),
            "F" => (Keycode::F, true),
            "G" => (Keycode::G, true),
            "H" => (Keycode::H, true),
            "I" => (Keycode::I, true),
            "J" => (Keycode::J, true),
            "K" => (Keycode::K, true),
            "L" => (Keycode::L, true),
            "M" => (Keycode::M, true),
            "N" => (Keycode::N, true),
            "O" => (Keycode::O, true),
            "P" => (Keycode::P, true),
            "Q" => (Keycode::Q, true),
            "R" => (Keycode::R, true),
            "S" => (Keycode::S, true),
            "T" => (Keycode::T, true),
            "U" => (Keycode::U, true),
            "V" => (Keycode::V, true),
            "W" => (Keycode::W, true),
            "X" => (Keycode::X, true),
            "Y" => (Keycode::Y, true),
            "Z" => (Keycode::Z, true),
            "!" => (Keycode::Num1, true),
            "@" => (Keycode::Num2, true),
            "#" => (Keycode::Num3, true),
            "$" => (Keycode::Num4, true),
            "%" => (Keycode::Num5, true),
            "^" => (Keycode::Num6, true),
            "&" => (Keycode::Num7, true),
            "*" => (Keycode::Num8, true),
            "(" => (Keycode::Num9, true),
            ")" => (Keycode::Num0, true),
            "RET" => (Keycode::Return, true),
            "ESC" => (Keycode::Escape, true),
            "BSP" => (Keycode::Backspace, true),
            "TAB" => (Keycode::Tab, true),
            "SPC" => (Keycode::Space, true),
            "_" => (Keycode::Minus, true),
            "+" => (Keycode::Equals, true),
            "{" => (Keycode::LeftBracket, true),
            "}" => (Keycode::RightBracket, true),
            "|" => (Keycode::Backslash, true),
            ":" => (Keycode::Semicolon, true),
            "\"" => (Keycode::Quote, true),
            "~" => (Keycode::Backquote, true),
            "<" => (Keycode::Comma, true),
            ">" => (Keycode::Period, true),
            "?" => (Keycode::Slash, true),
            "CAPS" => (Keycode::CapsLock, true),
            "F1" => (Keycode::F1, true),
            "F2" => (Keycode::F2, true),
            "F3" => (Keycode::F3, true),
            "F4" => (Keycode::F4, true),
            "F5" => (Keycode::F5, true),
            "F6" => (Keycode::F6, true),
            "F7" => (Keycode::F7, true),
            "F8" => (Keycode::F8, true),
            "F9" => (Keycode::F9, true),
            "F10" => (Keycode::F10, true),
            "F11" => (Keycode::F11, true),
            "F12" => (Keycode::F12, true),

            "a" => (Keycode::A, false),
            "b" => (Keycode::B, false),
            "c" => (Keycode::C, false),
            "d" => (Keycode::D, false),
            "e" => (Keycode::E, false),
            "f" => (Keycode::F, false),
            "g" => (Keycode::G, false),
            "h" => (Keycode::H, false),
            "i" => (Keycode::I, false),
            "j" => (Keycode::J, false),
            "k" => (Keycode::K, false),
            "l" => (Keycode::L, false),
            "m" => (Keycode::M, false),
            "n" => (Keycode::N, false),
            "o" => (Keycode::O, false),
            "p" => (Keycode::P, false),
            "q" => (Keycode::Q, false),
            "r" => (Keycode::R, false),
            "s" => (Keycode::S, false),
            "t" => (Keycode::T, false),
            "u" => (Keycode::U, false),
            "v" => (Keycode::V, false),
            "w" => (Keycode::W, false),
            "x" => (Keycode::X, false),
            "y" => (Keycode::Y, false),
            "z" => (Keycode::Z, false),
            "1" => (Keycode::Num1, false),
            "2" => (Keycode::Num2, false),
            "3" => (Keycode::Num3, false),
            "4" => (Keycode::Num4, false),
            "5" => (Keycode::Num5, false),
            "6" => (Keycode::Num6, false),
            "7" => (Keycode::Num7, false),
            "8" => (Keycode::Num8, false),
            "9" => (Keycode::Num9, false),
            "0" => (Keycode::Num0, false),
            "ret" => (Keycode::Return, false),
            "esc" => (Keycode::Escape, false),
            "bsp" => (Keycode::Backspace, false),
            "tab" => (Keycode::Tab, false),
            "spc" => (Keycode::Space, false),
            "-" => (Keycode::Minus, false),
            "=" => (Keycode::Equals, false),
            "[" => (Keycode::LeftBracket, false),
            "]" => (Keycode::RightBracket, false),
            "\\" => (Keycode::Backslash, false),
            ";" => (Keycode::Semicolon, false),
            "'" => (Keycode::Quote, false),
            "`" => (Keycode::Backquote, false),
            "," => (Keycode::Comma, false),
            "." => (Keycode::Period, false),
            "/" => (Keycode::Slash, false),
            "caps" => (Keycode::CapsLock, false),
            "f1" => (Keycode::F1, false),
            "f2" => (Keycode::F2, false),
            "f3" => (Keycode::F3, false),
            "f4" => (Keycode::F4, false),
            "f5" => (Keycode::F5, false),
            "f6" => (Keycode::F6, false),
            "f7" => (Keycode::F7, false),
            "f8" => (Keycode::F8, false),
            "f9" => (Keycode::F9, false),
            "f10" => (Keycode::F10, false),
            "f11" => (Keycode::F11, false),
            "f12" => (Keycode::F12, false),

            // Keycode::PrintScreen,
            // Keycode::ScrollLock,
            // Keycode::Pause,
            // Keycode::Insert,
            // Keycode::Home,
            // Keycode::PageUp,
            // Keycode::Delete,
            // Keycode::End,
            // Scancode::PageDown,
            // Scancode::Right,
            // Scancode::Left,
            // Scancode::Down,
            // Scancode::Up,
            _ => continue,
        };
        let mut mods = Mod::empty();
        if shift {
            mods |= Mod::LSHIFTMOD;
        }
        if ctrl {
            mods |= Mod::LCTRLMOD;
        }
        keys.push(Key { scancode, mods });
    }
    keys
}

struct LoadedFont {
    size: i32,
    glyphs: Vec<Glyph>,
    max_advance: (i16, i16),
    ascent: i32,
}

impl LoadedFont {}

fn hex_code_to_color(code: &str) -> Color {
    let r = u8::from_str_radix(&code[0..2], 16).unwrap();
    let g = u8::from_str_radix(&code[2..4], 16).unwrap();
    let b = u8::from_str_radix(&code[4..6], 16).unwrap();
    Color {
        r: r as f32 / 255.0,
        g: g as f32 / 255.0,
        b: b as f32 / 255.0,
    }
}

fn srgb_to_rgb(color: &Color) -> Color {
    Color {
        r: color.r.powf(2.2),
        g: color.g.powf(2.2),
        b: color.b.powf(2.2),
    }
}

struct DrawingContext<'a> {
    gl: glow::Context,
    video: sdl2::VideoSubsystem,
    quad_shader: glow::Program,
    text_shader: glow::Program,
    current_font: &'a LoadedFont,
    window: sdl2::video::Window,
}

fn parse_keymap<'a>(
    commands: &HashMap<&str, EditorOperation<'a>>,
    input: &[(&str, &str)],
) -> Vec<(Vec<Key>, EditorOperation<'a>)> {
    let mut normal_keymap = Vec::new();
    for mapped in input {
        normal_keymap.push((
            string_to_keys(mapped.0),
            commands
                .get(mapped.1)
                .expect(format!("command doesnt exist: {}", mapped.1).as_str())
                .to_owned(),
        ));
    }
    normal_keymap
}

impl<'a> DrawingContext<'a> {
    fn draw_glyph(&self, ch: char, pos: IVec2, color: &Color) {
        if (ch as usize) < 32 {
            dbg!(ch);
        }
        let glyph = &self.current_font.glyphs[ch as usize - 32];
        let window_size = self.window.drawable_size();
        let block_size = self.current_font.max_advance;
        let pen = (pos.x as f32, pos.y as f32);
        // println!("{:?}", pen);

        unsafe {
            self.gl.bind_texture(glow::TEXTURE_2D, Some(glyph.tex));
            // println!("{}", self.current_font.ascent);
            // println!("{:?}", ((pen.0 + glyph.bearing.0 as f32), (pen.1 + (glyph.y - glyph.bearing.1 + self.current_font.ascent) as f32)));
            self.gl.uniform_2_f32(
                self.gl
                    .get_uniform_location(self.text_shader, "offset")
                    .as_ref(),
                (pen.0 + glyph.bearing.0 as f32) / window_size.0 as f32,
                (pen.1 + (glyph.y + self.current_font.ascent - glyph.bearing.1) as f32)
                    / window_size.1 as f32,
            );
            self.gl.uniform_2_f32(
                self.gl
                    .get_uniform_location(self.text_shader, "scale")
                    .as_ref(),
                (glyph.x as f32) / window_size.0 as f32,
                (-glyph.y as f32) / window_size.1 as f32,
            );
            self.gl.uniform_4_f32(
                self.gl
                    .get_uniform_location(self.text_shader, "color")
                    .as_ref(),
                color.r,
                color.g,
                color.b,
                1.0,
            );
            self.gl.draw_arrays(glow::TRIANGLES, 0, 6);
        }
    }

    fn draw_glyph_on_grid(&self, ch: char, pos: (i32, i32), color: &Color) {
        let block_size = self.current_font.max_advance;
        let pen = (
            (pos.0 * block_size.0 as i32) as i32,
            (pos.1 * block_size.1 as i32) as i32,
        );

        self.draw_glyph(ch, IVec2 { x: pen.0, y: pen.1 }, color);
    }

    fn draw_text(&self, text: &str, mut pos: IVec2, color: &Color) -> IVec2 {
        for i in text.chars() {
            self.draw_glyph(i, pos.clone(), color);
            pos.x += self.current_font.max_advance.0 as i32;
        }
        pos
    }
}

#[derive(Clone)]
enum EditorOperation<'a> {
    Simple(&'a dyn Fn(&mut EditorState)),
    Motion(&'a dyn Fn(&mut EditorState, IVec2) -> Position),
}

fn create_syntax_theme(scheme: &CColorscheme) -> Theme {
    let themedef = vec![
        ("string", &scheme.colors[&scheme.syntax.strings]),
        ("constant.numeric", &scheme.colors[&scheme.syntax.numbers]),
        ("storage.type, keyword", &scheme.colors[&scheme.syntax.keywords]),
        ("support.type, entity.name.struct", &scheme.colors[&scheme.syntax.types]),
        ("support.macro", &scheme.colors[&scheme.syntax.macros]),
        ("keyword.operator", &scheme.colors[&scheme.syntax.operators]),
        ("support.function, entity.name.function, meta.require", &scheme.colors[&scheme.syntax.functions]),
        ("support.function.any-method, variable.function", &scheme.colors[&scheme.syntax.methods]),
        ("comment", &scheme.colors[&scheme.syntax.comments]),
        ("", &scheme.colors[&scheme.syntax.text]),
    ];

    let theme_items: Vec<ThemeItem> = themedef
        .iter()
        .map(|def| ThemeItem {
            scope: ScopeSelectors::from_str(def.0).unwrap(),
            style: StyleModifier {
                foreground: Some((&hex_code_to_color(&def.1)).into()),
                background: None,
                font_style: None,
            },
        })
        .collect();

    Theme {
        name: Some("default".to_string()),
        author: None,
        settings: ThemeSettings::default(),
        scopes: theme_items,
    }
}

fn main() {
    let sdl = sdl2::init().unwrap();
    let video = sdl.video().unwrap();
    let gl_attr = video.gl_attr();
    gl_attr.set_context_profile(sdl2::video::GLProfile::Core);
    gl_attr.set_context_version(3, 0);
    let window = video
        .window("space", 640, 480)
        .opengl()
        .resizable()
        .build()
        .unwrap();
    let gl_context = window.gl_create_context().unwrap();
    let gl = unsafe {
        glow::Context::from_loader_function(|s| video.gl_get_proc_address(s) as *const _)
    };
    let mut event_loop = sdl.event_pump().unwrap();

    // gl_attr.set_multisample_samples(4);

    let vertex_buffer = unsafe { gl.create_vertex_array().expect("uh nuh uh") };

    let test_shader = create_shader(&gl, include_str!("test.vert"), include_str!("test.frag"));

    let quad_shader = create_shader(&gl, include_str!("quad.vert"), include_str!("quad.frag"));
    let text_shader = create_shader(&gl, include_str!("quad.vert"), include_str!("text.frag"));

    let config: EditorConfig = toml::from_str(
        std::fs::read_to_string("/home/robin/.config/space/config.toml")
            .unwrap()
            .as_str(),
    )
    .unwrap();

    let mut scontext = swash::scale::ScaleContext::new();
    let file = std::fs::read(get_res_file_path("fonts/ttf/JetBrainsMono-Regular.ttf")).unwrap();
    let font_size = config.text_size as f32;
    let font = swash::FontRef::from_index(&file, 0).unwrap();
    let mut scaler = scontext.builder(font).size(font_size).hint(false).build();

    let mut shape_context = swash::shape::ShapeContext::new();
    let mut shaper = shape_context
        .builder(font)
        .script(swash::text::Script::Latin)
        .direction(swash::shape::Direction::LeftToRight)
        .size(font_size)
        .build();

    // println!("front:{:?}", font.localized_strings());
    // for string in font.localized_strings() {
    //     println!("[{:?}] {}", string.id(), string.to_string());
    // }

    let mut glyphs = Vec::new();
    // let glyph = font.charmap().map(33 as u32);
    // let bitmap =
    //     swash::scale::Render::new(&[
    //     Source::Bitmap(swash::scale::StrikeWith::BestFit),
    //     Source::Outline
    // ]).format(swash::zeno::Format::Alpha).render(&mut scaler, glyph).unwrap();
    let metrics = font.metrics(&[]).scale(font_size);
    println!("{:?}", metrics);
    let max_advance = (
        metrics.max_width as i16,
        (metrics.ascent + metrics.descent) as i16,
    );
    for i in 32..128 {
        let glyph = font.charmap().map(i as u32);
        let bitmap = swash::scale::Render::new(&[
            Source::Bitmap(swash::scale::StrikeWith::BestFit),
            Source::Outline,
        ])
        .format(swash::zeno::Format::Alpha)
        .render(&mut scaler, glyph)
        .unwrap();
        // let bitmap = scaler.scale_color_bitmap(glyph, swash::scale::StrikeWith::BestFit);

        unsafe {
            gl.pixel_store_i32(glow::UNPACK_ALIGNMENT, 1);
            let tex = gl.create_texture().unwrap();
            gl.bind_texture(glow::TEXTURE_2D, Some(tex));
            gl.tex_image_2d(
                glow::TEXTURE_2D,
                0,
                glow::R8 as i32,
                bitmap.placement.width as i32,
                bitmap.placement.height as i32,
                0,
                glow::RED,
                glow::UNSIGNED_BYTE,
                Some(bitmap.data.as_ref()),
            );
            gl.tex_parameter_i32(
                glow::TEXTURE_2D,
                glow::TEXTURE_MAG_FILTER,
                glow::LINEAR as i32,
            );
            gl.tex_parameter_i32(
                glow::TEXTURE_2D,
                glow::TEXTURE_MIN_FILTER,
                glow::LINEAR as i32,
            );
            glyphs.push(Glyph {
                tex,
                bearing: (bitmap.placement.left, bitmap.placement.top),
                x: bitmap.placement.width as i32,
                y: bitmap.placement.height as i32,
            });
        }
    }

    let font = LoadedFont {
        size: font_size as i32,
        glyphs,
        max_advance,
        ascent: metrics.ascent as i32,
    };

    let drawing_context = DrawingContext {
        gl,
        video,
        quad_shader,
        text_shader,
        current_font: &font,
        window,
    };

    let mut quit = false;
    // let mut pos = [0.0, 0.0];
    // let mut vel = [0.4, 1.0];
    let mut old = std::time::Instant::now();

    drawing_context.video.text_input().start();
    drawing_context.video.text_input().stop();

    let scheme: CColorscheme = toml::from_str(
        std::fs::read_to_string(get_res_file_path(format!("colorschemes/{}.toml", &config.colorscheme).as_str()))
            .ok()
            .expect(&format!(
                "colorscheme \"{}\" does not exist",
                config.colorscheme
            ))
            .as_str(),
    )
    .map_err(|a| panic!("{} in {}.toml", a.message(), config.colorscheme))
    .unwrap();

    let ps = SyntaxSet::load_defaults_nonewlines();

    let syntax = ps.find_syntax_by_extension("rs").unwrap();

    let mut editor = EditorState {
        filename: None,
        contents: vec![String::new()],
        view: IVec2::new(0, 0),
        cursor: IVec2::new(0, 0),
        mode: EditorMode::Normal,
        argument: String::new(),
        drawing_context: &drawing_context,
        command_amount: String::new(),
        config,
        colorscheme: scheme.into(),
        syntax: syntax.to_owned(),
        syntax_set: ps,
        undo_history: UndoHistory { 
            edits: Vec::new(),
            pos: 0,
        }
    };

    unsafe {
        drawing_context.gl.use_program(Some(text_shader));
        let background_color_rgb = srgb_to_rgb(&editor.colorscheme.ui.background);
        drawing_context.gl.clear_color(
            background_color_rgb.r,
            background_color_rgb.g,
            background_color_rgb.b,
            1.0,
        );

        drawing_context.gl.enable(glow::FRAMEBUFFER_SRGB);
    }

    let mut commands: HashMap<&str, EditorOperation> = HashMap::new();
    commands.insert("left", EditorOperation::Motion(&|a, b| a.left(b)));
    commands.insert("right", EditorOperation::Motion(&|a, b| a.right(b)));
    commands.insert("up",
        EditorOperation::Motion(&|a, b| {
            if matches!(a.mode, EditorMode::OperatorPending(..)) {
                a.up_whole_line(b)
            } else {
                a.up(b)
            }
        }),
    );
    commands.insert("down",
        EditorOperation::Motion(&|a, b| {
            if matches!(a.mode, EditorMode::OperatorPending(..)) {
                a.down_whole_line(b)
            } else {
                a.down(b)
            }
        }),
    );
    commands.insert("open_below", EditorOperation::Simple(&|a| a.open_below()));
    commands.insert("open_above", EditorOperation::Simple(&|a| a.open_above()));
    commands.insert("insert", EditorOperation::Simple(&|a| a.insert_mode()));
    commands.insert("insert_after",
        EditorOperation::Simple(&|a| a.insert_mode_after()),
    );
    commands.insert("word",
        EditorOperation::Motion(&|a, _| a.repeat(|c, d| c.next_word(d))),
    );
    commands.insert("word_backwards",
        EditorOperation::Motion(&|a, _| a.repeat(|c, d| c.previous_word(d))),
    );
    commands.insert("backspace", EditorOperation::Simple(&|a| a.backspace()));
    commands.insert("newline_at_cursor",
        EditorOperation::Simple(&|a| a.newline_at_cursor()),
    );
    commands.insert("normal_mode", EditorOperation::Simple(&|a| a.normal_mode()));
    commands.insert("save_file", EditorOperation::Simple(&|a| a.save_file()));
    commands.insert("open_file_mode",
        EditorOperation::Simple(&|a| a.open_file_mode()),
    );
    commands.insert("line_start",
        EditorOperation::Motion(&|a, b| a.line_start(b)),
    );
    commands.insert("line_end", EditorOperation::Motion(&|a, b| a.line_end(b)));
    commands.insert("first_line",
        EditorOperation::Motion(&|a, b| a.first_line(b)),
    );
    commands.insert("last_line", EditorOperation::Motion(&|a, b| a.last_line(b)));
    commands.insert("delete_mode", EditorOperation::Simple(&|a| a.delete_mode()));
    commands.insert("change_mode", EditorOperation::Simple(&|a| a.change_mode()));
    commands.insert("run_arg",
        EditorOperation::Simple(&|a| {
            if let EditorMode::ArgumentPending(_, f) = a.mode {
                a.normal_mode();
                f(a);
                a.argument.clear();
            } else if let EditorMode::SecArgumentPending(_, f) = a.mode {
                a.normal_mode();
                f(a);
                a.argument.clear();
            }
        }),
    );
    commands.insert("bsp_arg",
        EditorOperation::Simple(&|a| {
            a.argument.pop();
        }),
    );
    commands.insert("view_up", EditorOperation::Simple(&|a| a.view_up(5)));
    commands.insert("view_down", EditorOperation::Simple(&|a| a.view_down(5)));
    commands.insert("insert_tab", EditorOperation::Simple(&|a| a.insert("\t")));
    commands.insert("current_line",
        EditorOperation::Motion(&|a, b| a.current_line_m(b)),
    );
    commands.insert("copy", EditorOperation::Simple(&|a| a.copy()));
    commands.insert("paste", EditorOperation::Simple(&|a| a.paste()));
    commands.insert("undo", EditorOperation::Simple(&|a| a.undo()));
    commands.insert("redo", EditorOperation::Simple(&|a| a.redo()));
    commands.insert("load_config", EditorOperation::Simple(&|a| a.load_config()));
    commands.insert("1",
        EditorOperation::Simple(&|a| a.command_amount.push('1')),
    );
    commands.insert("2",
        EditorOperation::Simple(&|a| a.command_amount.push('2')),
    );
    commands.insert("3",
        EditorOperation::Simple(&|a| a.command_amount.push('3')),
    );
    commands.insert("4",
        EditorOperation::Simple(&|a| a.command_amount.push('4')),
    );
    commands.insert("5",
        EditorOperation::Simple(&|a| a.command_amount.push('5')),
    );
    commands.insert("6",
        EditorOperation::Simple(&|a| a.command_amount.push('6')),
    );
    commands.insert("7",
        EditorOperation::Simple(&|a| a.command_amount.push('7')),
    );
    commands.insert("8",
        EditorOperation::Simple(&|a| a.command_amount.push('8')),
    );
    commands.insert("9",
        EditorOperation::Simple(&|a| a.command_amount.push('9')),
    );
    commands.insert("0",
        EditorOperation::Simple(&|a| a.command_amount.push('0')),
    );

    let normal_keymap = parse_keymap(
        &commands,
        &[
            // TODO translate move commands into the mode-appropriate versions
            // specific versions will be accessible through nmove_left omove_left etc.
            ("n", "left"),
            ("e", "down"),
            ("i", "up"),
            ("o", "right"),
            ("C-e", "view_down"),
            ("C-i", "view_up"),
            ("l", "open_below"),
            ("L", "open_above"),
            ("u", "insert"),
            ("a", "insert_after"),
            ("w", "word"),
            ("b", "word_backwards"),
            ("d", "delete_mode"),
            ("c", "change_mode"),
            ("spc s", "save_file"),
            ("spc spc", "open_file_mode"),
            ("spc r", "load_config"),
            ("g n", "line_start"),
            ("g o", "line_end"),
            ("g e", "last_line"),
            ("g i", "first_line"),
            ("k", "undo"),
            ("K", "redo"),
            ("p", "paste"),
            ("y", "copy"),
            ("1", "1"),
            ("2", "2"),
            ("3", "3"),
            ("4", "4"),
            ("5", "5"),
            ("6", "6"),
            ("7", "7"),
            ("8", "8"),
            ("9", "9"),
            ("0", "0"),
        ],
    );

    let insert_keymap = parse_keymap(
        &commands,
        &[
            ("ret", "newline_at_cursor"),
            ("bsp", "backspace"),
            ("esc", "normal_mode"),
            ("tab", "insert_tab"),
        ],
    );

    let operator_keymap = parse_keymap(
        &commands,
        &[
            ("w", "word"),
            ("b", "word_backwards"),
            ("e", "down"),
            ("i", "up"),
            ("g n", "line_start"),
            ("g o", "line_end"),
            ("g e", "last_line"),
            ("g i", "first_line"),
            ("d", "current_line"),
            ("1", "1"),
            ("2", "2"),
            ("3", "3"),
            ("4", "4"),
            ("5", "5"),
            ("6", "6"),
            ("7", "7"),
            ("8", "8"),
            ("9", "9"),
            ("0", "0"),
        ],
    );

    let argument_keymap = parse_keymap(&commands, &[("ret", "run_arg"), ("bsp", "bsp_arg")]);

    if cfg!(debug_assertions) {
        editor.load_file("test.rs");
    } else {
        let args = std::env::args().collect::<Vec<String>>();
        if args.len() > 1 {
            editor.load_file(&args[1]);
        }
    }

    let (sender, receiver) = std::sync::mpsc::channel();
    let mut watcher = notify::recommended_watcher(sender).unwrap();
    let home = if cfg!(target_os = "windows") {
        // windows is *special*
        std::env::var("USERPROFILE").unwrap()
    } else {
        std::env::var("HOME").unwrap()
    };
    watcher
        .watch(
            std::path::Path::new((home + "/.config/space/config.toml").as_str()),
            RecursiveMode::NonRecursive,
        )
        .unwrap();

    let mut current_command = Vec::new();

    while !quit {
        // hot reload config
        for _ in receiver.try_iter() {
            editor.load_config();
        }

        let delta = old.elapsed().as_secs_f32();
        old = std::time::Instant::now();
        for event in event_loop.poll_iter() {
            match event {
                sdl2::event::Event::Quit { .. } => quit = true,
                Event::KeyDown {
                    timestamp,
                    window_id,
                    keycode,
                    scancode,
                    keymod,
                    repeat,
                } => {
                    match keycode.unwrap() {
                        Keycode::LAlt
                        | Keycode::LGui
                        | Keycode::LCtrl
                        | Keycode::RGui
                        | Keycode::RAlt
                        | Keycode::LShift
                        | Keycode::RShift
                        | Keycode::RCtrl => {
                            continue;
                        }
                        _ => {}
                    }
                    let key = Key {
                        scancode: keycode.unwrap(),
                        mods: keymod,
                    };

                    current_command.push(key.clone());
                    // println!("{:?}", current_command);
                    fn match_current_command(
                        editor: &mut EditorState,
                        keymap: &Vec<(Vec<Key>, EditorOperation)>,
                        current_command: &mut Vec<Key>,
                    ) {
                        let mut found = false;
                        for m in keymap.iter() {
                            if m.0.len() >= current_command.len()
                                && &m.0[..current_command.len()] == current_command
                            {
                                found = true;
                            }
                            if &m.0 == current_command {
                                match m.1 {
                                    EditorOperation::Simple(f) => {
                                        f(editor);
                                    }
                                    EditorOperation::Motion(f) => {
                                        let pos = f(editor, editor.cursor.clone());
                                        // println!("{:?}", pos);
                                        match editor.mode {
                                            EditorMode::Normal => {
                                                // clamp cursor pos
                                                // let clamped_pos = editor.normalize_pos(pos);
                                                editor._move(pos);
                                            }
                                            EditorMode::OperatorPending(f_) => {
                                                editor.normal_mode();
                                                f_(editor, pos.clone());
                                                let move_part = editor.move_part(pos.clone());
                                                if move_part.x < editor.cursor.x
                                                    || move_part.y < editor.cursor.y
                                                {
                                                    editor._move(pos);
                                                }
                                            }
                                            _ => {
                                                editor._move(pos);
                                            }
                                        }
                                    }
                                }
                                current_command.clear();
                            }
                        }
                        if !found {
                            current_command.clear();
                            if matches!(editor.mode, EditorMode::OperatorPending(..)) {
                                editor.normal_mode();
                            }
                        }
                    }
                    match editor.mode {
                        EditorMode::Normal => {
                            match_current_command(
                                &mut editor,
                                &normal_keymap,
                                &mut current_command,
                            );
                        }
                        EditorMode::Insert => {
                            match_current_command(
                                &mut editor,
                                &insert_keymap,
                                &mut current_command,
                            );
                        }
                        EditorMode::OperatorPending(f) => {
                            match_current_command(
                                &mut editor,
                                &operator_keymap,
                                &mut current_command,
                            );
                        }
                        EditorMode::ArgumentPending(_, f) | EditorMode::SecArgumentPending(_, f) => {
                            match_current_command(
                                &mut editor,
                                &argument_keymap,
                                &mut current_command,
                            );
                        }
                        _ => {}
                    }
                }
                Event::TextInput {
                    timestamp,
                    window_id,
                    text,
                } => match editor.mode {
                    EditorMode::Insert => {
                        editor.insert(&text);
                    }
                    EditorMode::ArgumentPending(..) | EditorMode::SecArgumentPending(..) => {
                        editor.argument.push_str(&text);
                    }
                    _ => {}
                },
                Event::Window { win_event, .. } => match win_event {
                    WindowEvent::Resized(x, y) => unsafe {
                        drawing_context.gl.viewport(0, 0, x, y)
                    },
                    _ => {}
                },
                _ => {}
            }
        }

        // pos[0] += delta * vel[0];
        // pos[1] += delta * vel[1];

        // for (i, v) in pos.iter_mut().enumerate() {
        //     if *v > 1.0 {
        //         vel[i] = -vel[i];
        //         *v = 1.0;
        //     }
        //     if *v < 0.0 {
        //         vel[i] = -vel[i];
        //         *v = 0.0;
        //     }
        // }

        unsafe {
            drawing_context.gl.clear(glow::COLOR_BUFFER_BIT);

            let window_size = drawing_context.window.drawable_size();

            let block_size = (max_advance.0 as i32, max_advance.1 as i32);

            let cursor_mode = match editor.mode {
                EditorMode::Normal => CursorMode::Block,
                EditorMode::Insert => CursorMode::Bar,
                EditorMode::OperatorPending(_) => CursorMode::Underline,
                EditorMode::ArgumentPending(_, _) => CursorMode::Bar,
                EditorMode::SecArgumentPending(_, _) => CursorMode::Bar,
            };

            let cursor_size = match cursor_mode {
                CursorMode::Block => IVec2::new(block_size.0, block_size.1),
                CursorMode::Underline => IVec2::new(block_size.0, 4),
                CursorMode::Bar => IVec2::new(2, block_size.1),
                _ => IVec2::new(2, block_size.1),
            };

            let cursor_offset = match cursor_mode {
                CursorMode::Underline => IVec2::new(0, block_size.1 - 4),
                _ => IVec2::new(0, 0),
            };

            // println!("{}", max_advance);

            let pen_x = 5;

            // drawing_context.gl.use_program(Some(quad_shader));

            drawing_context.gl.uniform_2_f32(
                drawing_context
                    .gl
                    .get_uniform_location(quad_shader, "offset")
                    .as_ref(),
                0.0,
                (block_size.1 * (editor.cursor.y - editor.view.y)) as f32 / window_size.1 as f32,
            );
            drawing_context.gl.uniform_2_f32(
                drawing_context
                    .gl
                    .get_uniform_location(quad_shader, "scale")
                    .as_ref(),
                1.0,
                (block_size.1) as f32 / window_size.1 as f32,
            );
            drawing_context.gl.uniform_4_f32(
                drawing_context
                    .gl
                    .get_uniform_location(quad_shader, "color")
                    .as_ref(),
                editor.colorscheme.ui.current_line.r,
                editor.colorscheme.ui.current_line.g,
                editor.colorscheme.ui.current_line.b,
                1.0,
            );
            drawing_context.gl.draw_arrays(glow::TRIANGLES, 0, 6);

            let mut cursor_x = 0;

            for (i, ch) in editor.get_line(editor.cursor.y).chars().enumerate() {
                if i as i32 >= editor.cursor.x {
                    break;
                }
                match ch {
                    '\t' => {
                        cursor_x += editor.config.tab_width;
                    }
                    _ => {
                        cursor_x += 1;
                    }
                }
            }

            drawing_context.gl.uniform_2_f32(
                drawing_context
                    .gl
                    .get_uniform_location(quad_shader, "offset")
                    .as_ref(),
                ((block_size.0 * cursor_x as i32 + pen_x as i32 * block_size.0 + cursor_offset.x)
                    as f32)
                    / window_size.0 as f32,
                (block_size.1 * (editor.cursor.y - editor.view.y) + cursor_offset.y) as f32
                    / window_size.1 as f32,
            );
            drawing_context.gl.uniform_2_f32(
                drawing_context
                    .gl
                    .get_uniform_location(quad_shader, "scale")
                    .as_ref(),
                (cursor_size.x) as f32 / window_size.0 as f32,
                (cursor_size.y) as f32 / window_size.1 as f32,
            );
            drawing_context.gl.uniform_4_f32(
                drawing_context
                    .gl
                    .get_uniform_location(quad_shader, "color")
                    .as_ref(),
                editor.colorscheme.ui.cursor.r,
                editor.colorscheme.ui.cursor.g,
                editor.colorscheme.ui.cursor.b,
                1.0,
            );
            drawing_context.gl.draw_arrays(glow::TRIANGLES, 0, 6);

            drawing_context.gl.use_program(Some(text_shader));
            drawing_context.gl.enable(glow::BLEND);
            drawing_context
                .gl
                .blend_func(glow::SRC_ALPHA, glow::ONE_MINUS_SRC_ALPHA);

            for i in 0 + editor.view.y
                ..(window_size.1 as i32 / block_size.1 + editor.view.y)
                    .min(editor.contents.len() as i32)
            {
                let (line_num, color) = if editor.cursor.y - i == 0 {
                    (editor.cursor.y + 1, &editor.colorscheme.ui.foreground)
                } else {
                    (
                        if editor.config.relative_line_numbers {
                            editor.cursor.y - i
                        } else {
                            editor.view.y + i + 1
                        },
                        &editor.colorscheme.ui.unfocused_text,
                    )
                };
                let line_str = line_num.abs().to_string();
                for (p, ch) in line_str.chars().rev().enumerate() {
                    drawing_context.draw_glyph_on_grid(
                        ch,
                        ((pen_x - 2) - p as i32, i as i32 - editor.view.y),
                        color,
                    );
                }
            }

            // let mut pen = (0.5 * window_size.0 as f32, 0.5 * window_size.1 as f32);
            let mut pen = (pen_x, 0);

            let scale = 1.0; //15.0 / 64.0;

            for (i, ch) in editor.command_amount.chars().enumerate() {
                drawing_context.draw_glyph_on_grid(
                    ch,
                    (i as i32, (window_size.1 as i32 / cursor_size.y) - 1),
                    &editor.colorscheme.ui.foreground,
                )
            }
            // println!("chud {}", editor.command_amount);

            let mut h = HighlightLines::new(&editor.syntax, &editor.colorscheme.syntax_theme);

            for line in editor
                .contents
                .iter()
                .skip(editor.view.y as usize)
                .take(window_size.1 as usize / block_size.1 as usize)
            {
                let ranges: Vec<(Style, &str)> = h.highlight_line(line, &editor.syntax_set).unwrap();
                let mut char_pos = 0;
                for range in ranges {
                    for i in range.1.chars() {
                        match i {
                            '\t' => {
                                pen.0 += editor.config.tab_width;
                            }
                            _ => {
                                if char_pos == editor.cursor.x
                                    && pen.1 == editor.cursor.y
                                    && cursor_mode == CursorMode::Block
                                {
                                    drawing_context.draw_glyph_on_grid(i, pen, &editor.colorscheme.ui.background);
                                } else {
                                    drawing_context.draw_glyph_on_grid(i, pen, &range.0.foreground.into());
                                }
                                pen.0 += 1;
                            }
                        }
                        char_pos += 1;
                    }
                }
                pen.1 += 1;
                // pen.1 += max_advance as f32;
                pen.0 = pen_x;
            }
            if let EditorMode::ArgumentPending(ref query_str, _) = editor.mode {
                pen.1 = window_size.1 as i32 - max_advance.1 as i32;
                pen.0 = 0;
                for i in query_str.chars() {
                    drawing_context.draw_glyph(i, IVec2 { x: pen.0, y: pen.1 }, &Color { r: 255.0, g: 255.0, b: 255.0 });
                    pen.0 += max_advance.0 as i32;
                }
                for i in editor.argument.chars() {
                    drawing_context.draw_glyph(i, IVec2 { x: pen.0, y: pen.1 }, &Color { r: 255.0, g: 255.0, b: 255.0 });
                    pen.0 += max_advance.0 as i32;
                }
            }

            if let EditorMode::SecArgumentPending(ref query_str, _) = editor.mode {
                pen.1 = window_size.1 as i32 - max_advance.1 as i32;
                pen.0 = 0;
                for i in query_str.chars() {
                    drawing_context.draw_glyph(i, IVec2 { x: pen.0, y: pen.1 }, &Color { r: 255.0, g: 255.0, b: 255.0 });
                    pen.0 += max_advance.0 as i32;
                }
                for i in editor.argument.chars() {
                    drawing_context.draw_glyph('*', IVec2 { x: pen.0, y: pen.1 }, &Color { r: 255.0, g: 255.0, b: 255.0 });
                    pen.0 += max_advance.0 as i32;
                }
            }

            drawing_context.gl.disable(glow::BLEND);

            drawing_context.window.gl_swap_window();
        }
    }
    unsafe {
        drawing_context.gl.delete_program(test_shader);
        drawing_context.gl.delete_vertex_array(vertex_buffer);
    }
}
