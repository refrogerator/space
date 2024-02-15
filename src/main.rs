use std::collections::HashMap;
use std::io::Write;

use glow::*;
use sdl2::event::{Event, WindowEvent};
use freetype::Library;
use freetype::face::LoadFlag;
use sdl2::keyboard::Scancode;
use sdl2::keyboard::Mod;

fn create_shader(gl: &glow::Context, vert: &str, frag: &str) -> glow::Program {
    unsafe {
        let program = gl.create_program().unwrap();
        let vs_source = std::fs::read_to_string(vert).unwrap();
        let fs_source = std::fs::read_to_string(frag).unwrap();
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

struct Vec2 {
    x: f32,
    y: f32
}

struct Vec3 {
    x: f32,
    y: f32,
    z: f32
}

struct Vec4 {
    x: f32,
    y: f32,
    z: f32,
    w: f32
}

struct Color {
    r: f32,
    g: f32,
    b: f32
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
            Offset::Auto => {
                1.0
            }
            Offset::Px(px) => {
                px / pixels
            }
            Offset::Percent(p) => {
                p / 100.0
            }
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
            _ => {
                1.0
            }
        }
    }
}

impl Layout {
    fn to_screen_coords(&self, container: Rect) -> Rect {
        let ratio = container.w / container.h;
        let mut ret = Rect { x: 0.0, y: 0.0, w: 0.0, h: 0.0 };

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
enum EditorMode {
    Normal,
    Insert
}

struct EditorState<'a, 'b> {
    contents: Option<Vec<String>>,
    filename: Option<String>,
    cursor: (i32, i32),
    mode: EditorMode,
    drawing_context: &'a DrawingContext<'b>,
}

impl <'a, 'b>EditorState<'a, 'b> {
    fn load_file(&mut self, filename: &str) {
        self.filename = Some(filename.to_string());
        self.contents = Some(std::fs::read_to_string(filename).unwrap().lines().map(|a| a.to_owned()).collect());
        self.cursor = (0, 0);
        self.mode = EditorMode::Normal;
    }

    fn save_file_as(&self, filename: &str) {
        if let Some(ref lines) = self.contents {
            let mut file = std::fs::File::create(filename).unwrap();
            for i in lines {
                file.write(i.as_bytes()).unwrap();
                file.write(b"\n").unwrap();
            }
        }
    }

    fn save_file(&self) {
        if let Some(ref filename) = self.filename {
            self.save_file_as(filename);
        }
    }

    fn move_up(&mut self) {
        if self.cursor.1 > 0 {
            self.cursor.1 -= 1;
            if self.get_current_line().len() == 0 {
                self.cursor.0 = 0;
            } else if self.cursor.0 >= self.get_current_line().len() as i32 {
                self.cursor.0 = self.get_current_line().len() as i32 - 1;
            }
        }
    }
    fn move_down(&mut self) {
        if let Some(ref contents) = self.contents {
            if self.cursor.1 < contents.len() as i32 - 1 {
                self.cursor.1 += 1;
                if self.get_current_line().len() == 0 {
                    self.cursor.0 = 0;
                } else if self.cursor.0 >= self.get_current_line().len() as i32 {
                    self.cursor.0 = self.get_current_line().len() as i32 - 1;
                }
            }
        }
    }
    fn move_left(&mut self) {
        if self.cursor.0 > 0 {
            self.cursor.0 -= 1;
        }
    }
    fn move_right_normal(&mut self) {
        if let Some(ref contents) = self.contents {
            if self.cursor.0 < contents[self.cursor.1 as usize].len() as i32 - 1 {
                self.cursor.0 += 1;
            }
        }
    }
    fn move_right_insert(&mut self) {
        if let Some(ref contents) = self.contents {
            if self.cursor.0 < contents[self.cursor.1 as usize].len() as i32 {
                self.cursor.0 += 1;
            }
        }
    }
    fn move_to_line_start(&mut self) {
        self.cursor.0 = 0;
    }

    fn move_to_line_end_normal(&mut self) {
        self.cursor.0 = self.get_current_line().len() as i32 - 1;
    }

    fn move_to_line_end(&mut self) {
        self.cursor.0 = self.get_current_line().len() as i32;
    }

    fn get_current_line_mut(&mut self) -> &mut String {
        if let Some(ref mut contents) = self.contents {
            return &mut contents[self.cursor.1 as usize];
        } else {
            panic!("no file loaded");
        }
    }

    fn get_current_line(&self) -> &str {
        if let Some(ref contents) = self.contents {
            return &contents[self.cursor.1 as usize];
        } else {
            panic!("no file loaded");
        }
    }

    fn next_word(&self) -> (i32, i32) {
        let line = self.get_current_line();
        let alpha = line.as_bytes()[self.cursor.0 as usize].is_ascii_alphanumeric();
        let mut first_word = alpha;
        for (i, v) in line.chars().skip(self.cursor.0 as usize + 1).enumerate() {
            if first_word {
                if !v.is_alphanumeric() {
                    first_word = false;
                }
            } else {
                if v.is_alphanumeric() {
                    return (self.cursor.0 + i as i32 + 1, self.cursor.1);
                }
            }
        }
        return (line.len() as i32 - 1, self.cursor.1);
    }

    fn previous_word(&mut self) -> (i32, i32) {
        let line = self.get_current_line();
        let alpha = line.as_bytes()[self.cursor.0 as usize].is_ascii_alphanumeric();
        let mut first_word = alpha;
        for (i, v) in line.chars().rev().skip(line.len() - (self.cursor.0 as usize)).enumerate() {
            if first_word {
                if !v.is_alphanumeric() {
                    first_word = false;
                }
            } else {
                if v.is_alphanumeric() {
                    return (self.cursor.0 - i as i32 - 1, self.cursor.1);
                }
            }
        }
        return (0, self.cursor.1);
    }

    fn move_word(&mut self) {
        self.cursor = self.next_word();
    }

    fn move_word_backwards(&mut self) {
        self.cursor = self.previous_word();
    }

    fn newline_at_cursor(&mut self) {
        if let Some(ref mut contents) = self.contents {
            let new_line = contents[self.cursor.1 as usize].split_off(self.cursor.0 as usize);
            contents.insert(self.cursor.1 as usize + 1, new_line);
            self.move_down();
            self.move_to_line_start();
        }
    }

    fn open_above(&mut self) {
        if let Some(ref mut contents) = self.contents {
            contents.insert(self.cursor.1 as usize, String::new());
            self.move_to_line_start();
            self.insert_mode();
        }
    }

    fn open_below(&mut self) {
        if let Some(ref mut contents) = self.contents {
            contents.insert(self.cursor.1 as usize + 1, String::new());
            self.move_down();
            self.move_to_line_start();
            self.insert_mode();
        }
    }

    fn insert(&mut self, text: &str) {
        if let Some(ref mut contents) = self.contents {
            contents[self.cursor.1 as usize].insert_str(self.cursor.0 as usize, text);
            self.move_right_insert();
        }
    }

    fn backspace(&mut self) {
        if let Some(ref mut contents) = self.contents {
            if self.cursor.0 > 0 {
                contents[self.cursor.1 as usize].remove(self.cursor.0 as usize - 1);
                self.move_left();
            } else {
                if contents[self.cursor.1 as usize].len() > 0 {
                    let line = contents.remove(self.cursor.1 as usize);
                    self.move_up();
                    self.move_to_line_end();
                    self.get_current_line_mut().push_str(&line);
                } else {
                    contents.remove(self.cursor.1 as usize);
                    self.move_up();
                    self.move_to_line_end();
                }
            }
        }
    }

    fn insert_mode(&mut self) {
        self.mode = EditorMode::Insert;
        self.drawing_context.video.text_input().start();
    }

    fn insert_mode_after(&mut self) {
        if let Some(ref mut contents) = self.contents {
            if self.cursor.0 < contents[self.cursor.1 as usize].len() as i32 {
                self.cursor.0 += 1;
            }
        }
        self.insert_mode();
    }

    fn normal_mode(&mut self) {
        if self.get_current_line().len() == 0 {
            self.cursor.0 = 0;
        } else if self.cursor.0 >= self.get_current_line().len() as i32 {
            self.cursor.0 = self.get_current_line().len() as i32 - 1;
        }
        self.mode = EditorMode::Normal;
        self.drawing_context.video.text_input().stop();
    }
}

#[derive(Clone, Debug, PartialEq)]
struct Key {
    scancode: sdl2::keyboard::Scancode,
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
        let keychord = if ctrl {
            &_keychord[2..]
        } else {
            _keychord
        };
        println!("{}, {}", keychord, ctrl);
        let (scancode, shift) = match keychord {
            "A" => { (Scancode::A, true) },
            "B" => { (Scancode::B, true) },
            "C" => { (Scancode::C, true) },
            "D" => { (Scancode::D, true) },
            "E" => { (Scancode::E, true) },
            "F" => { (Scancode::F, true) },
            "G" => { (Scancode::G, true) },
            "H" => { (Scancode::H, true) },
            "I" => { (Scancode::I, true) },
            "J" => { (Scancode::J, true) },
            "K" => { (Scancode::K, true) },
            "L" => { (Scancode::L, true) },
            "M" => { (Scancode::M, true) },
            "N" => { (Scancode::N, true) },
            "O" => { (Scancode::O, true) },
            "P" => { (Scancode::P, true) },
            "Q" => { (Scancode::Q, true) },
            "R" => { (Scancode::R, true) },
            "S" => { (Scancode::S, true) },
            "T" => { (Scancode::T, true) },
            "U" => { (Scancode::U, true) },
            "V" => { (Scancode::V, true) },
            "W" => { (Scancode::W, true) },
            "X" => { (Scancode::X, true) },
            "Y" => { (Scancode::Y, true) },
            "Z" => { (Scancode::Z, true) },
            "!" => { (Scancode::Num1, true) },
            "@" => { (Scancode::Num2, true) },
            "#" => { (Scancode::Num3, true) },
            "$" => { (Scancode::Num4, true) },
            "%" => { (Scancode::Num5, true) },
            "^" => { (Scancode::Num6, true) },
            "&" => { (Scancode::Num7, true) },
            "*" => { (Scancode::Num8, true) },
            "(" => { (Scancode::Num9, true) },
            ")" => { (Scancode::Num0, true) },
            "RET" => { (Scancode::Return, true) },
            "ESC" => { (Scancode::Escape, true) },
            "BSP" => { (Scancode::Backspace, true) },
            "TAB" => { (Scancode::Tab, true) },
            "SPC" => { (Scancode::Space, true) },
            "_" => { (Scancode::Minus, true) },
            "+" => { (Scancode::Equals, true) },
            "{" => { (Scancode::LeftBracket, true) },
            "}" => { (Scancode::RightBracket, true) },
            "|" => { (Scancode::Backslash, true) },
            ":" => { (Scancode::Semicolon, true) },
            "\"" => { (Scancode::Apostrophe, true) },
            "~" => { (Scancode::Grave, true) },
            "<" => { (Scancode::Comma, true) },
            ">" => { (Scancode::Period, true) },
            "?" => { (Scancode::Slash, true) },
            "CAPS" => { (Scancode::CapsLock, true) },
            "F1" => { (Scancode::F1, true) },
            "F2" => { (Scancode::F2, true) },
            "F3" => { (Scancode::F3, true) },
            "F4" => { (Scancode::F4, true) },
            "F5" => { (Scancode::F5, true) },
            "F6" => { (Scancode::F6, true) },
            "F7" => { (Scancode::F7, true) },
            "F8" => { (Scancode::F8, true) },
            "F9" => { (Scancode::F9, true) },
            "F10" => { (Scancode::F10, true) },
            "F11" => { (Scancode::F11, true) },
            "F12" => { (Scancode::F12, true) },

            "a" => { (Scancode::A, false) },
            "b" => { (Scancode::B, false) },
            "c" => { (Scancode::C, false) },
            "d" => { (Scancode::D, false) },
            "e" => { (Scancode::E, false) },
            "f" => { (Scancode::F, false) },
            "g" => { (Scancode::G, false) },
            "h" => { (Scancode::H, false) },
            "i" => { (Scancode::I, false) },
            "j" => { (Scancode::J, false) },
            "k" => { (Scancode::K, false) },
            "l" => { (Scancode::L, false) },
            "m" => { (Scancode::M, false) },
            "n" => { (Scancode::N, false) },
            "o" => { (Scancode::O, false) },
            "p" => { (Scancode::P, false) },
            "q" => { (Scancode::Q, false) },
            "r" => { (Scancode::R, false) },
            "s" => { (Scancode::S, false) },
            "t" => { (Scancode::T, false) },
            "u" => { (Scancode::U, false) },
            "v" => { (Scancode::V, false) },
            "w" => { (Scancode::W, false) },
            "x" => { (Scancode::X, false) },
            "y" => { (Scancode::Y, false) },
            "z" => { (Scancode::Z, false) },
            "1" => { (Scancode::Num1, false) },
            "2" => { (Scancode::Num2, false) },
            "3" => { (Scancode::Num3, false) },
            "4" => { (Scancode::Num4, false) },
            "5" => { (Scancode::Num5, false) },
            "6" => { (Scancode::Num6, false) },
            "7" => { (Scancode::Num7, false) },
            "8" => { (Scancode::Num8, false) },
            "9" => { (Scancode::Num9, false) },
            "0" => { (Scancode::Num0, false) },
            "ret" => { (Scancode::Return, false) },
            "esc" => { (Scancode::Escape, false) },
            "bsp" => { (Scancode::Backspace, false) },
            "tab" => { (Scancode::Tab, false) },
            "spc" => { (Scancode::Space, false) },
            "-" => { (Scancode::Minus, false) },
            "=" => { (Scancode::Equals, false) },
            "[" => { (Scancode::LeftBracket, false) },
            "]" => { (Scancode::RightBracket, false) },
            "\\" => { (Scancode::Backslash, false) },
            ";" => { (Scancode::Semicolon, false) },
            "'" => { (Scancode::Apostrophe, false) },
            "`" => { (Scancode::Grave, false) },
            "," => { (Scancode::Comma, false) },
            "." => { (Scancode::Period, false) },
            "/" => { (Scancode::Slash, false) },
            "caps" => { (Scancode::CapsLock, false) },
            "f1" => { (Scancode::F1, false) },
            "f2" => { (Scancode::F2, false) },
            "f3" => { (Scancode::F3, false) },
            "f4" => { (Scancode::F4, false) },
            "f5" => { (Scancode::F5, false) },
            "f6" => { (Scancode::F6, false) },
            "f7" => { (Scancode::F7, false) },
            "f8" => { (Scancode::F8, false) },
            "f9" => { (Scancode::F9, false) },
            "f10" => { (Scancode::F10, false) },
            "f11" => { (Scancode::F11, false) },
            "f12" => { (Scancode::F12, false) },
            // Scancode::PrintScreen,
            // Scancode::ScrollLock,
            // Scancode::Pause,
            // Scancode::Insert,
            // Scancode::Home,
            // Scancode::PageUp,
            // Scancode::Delete,
            // Scancode::End,
            // Scancode::PageDown,
            // Scancode::Right,
            // Scancode::Left,
            // Scancode::Down,
            // Scancode::Up,
            _ => { continue },
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
    max_advance: i16,
}

impl LoadedFont {
}

fn hex_code_to_color(code: &str) -> (f32, f32, f32) {
    let r = u8::from_str_radix(&code[0..2], 16).unwrap();
    let g = u8::from_str_radix(&code[2..4], 16).unwrap();
    let b = u8::from_str_radix(&code[4..6], 16).unwrap();
    (r as f32 / 255.0,
     g as f32 / 255.0,
     b as f32 / 255.0)
}

fn srgb_to_rgb(color: (f32, f32, f32)) -> (f32, f32, f32) {
    (
        color.0.powf(2.2),
        color.1.powf(2.2),
        color.2.powf(2.2)
    )
}

struct DrawingContext<'a> {
    gl: glow::Context,
    video: sdl2::VideoSubsystem,
    quad_shader: glow::Program,
    text_shader: glow::Program,
    current_font: &'a LoadedFont,
    window: sdl2::video::Window,
}

fn parse_keymap<'a>(commands: &HashMap<&str, &'a dyn Fn(&mut EditorState)>, input: &[(&str, &str)]) -> Vec<(Vec<Key>, &'a dyn Fn(&mut EditorState))> {
    let mut normal_keymap = Vec::new();
    for mapped in input {
        normal_keymap.push((string_to_keys(mapped.0), commands.get(mapped.1).expect("command doesnt exist").to_owned()));
    }
    normal_keymap
}

impl <'a>DrawingContext<'a> {
    fn draw_glyph_on_grid(&self, ch: char, pos: (i32, i32), color: (f32, f32, f32)) {
        let glyph = &self.current_font.glyphs[ch as usize - 32];
        let window_size = self.window.drawable_size();
        let block_size = (self.current_font.max_advance as i32 / 2 as i32, self.current_font.max_advance as i32);
        let pen = ((pos.0 * block_size.0) as f32, (pos.1 * block_size.1) as f32);

        unsafe {
            self.gl.bind_texture(glow::TEXTURE_2D, Some(glyph.tex));
            self.gl.uniform_2_f32(self.gl.get_uniform_location(self.text_shader, "offset").as_ref(), pen.0 / window_size.0 as f32, (pen.1 + (glyph.y - glyph.bearing.1) as f32 + 15.0) / window_size.1 as f32);
            self.gl.uniform_2_f32(self.gl.get_uniform_location(self.text_shader, "scale").as_ref(), (glyph.x as f32) / window_size.0 as f32, (-glyph.y as f32) / window_size.1 as f32);
            self.gl.uniform_4_f32(self.gl.get_uniform_location(self.text_shader, "color").as_ref(), color.0, color.1, color.2, 0.0);
            self.gl.draw_arrays(glow::TRIANGLES, 0, 6);
        }
    }
}

fn main() {
    let sdl = sdl2::init().unwrap();
    let video = sdl.video().unwrap();
    let gl_attr = video.gl_attr();
    gl_attr.set_context_profile(sdl2::video::GLProfile::Core);
    gl_attr.set_context_version(3, 0);
    let window = video.window("fortnite balls", 640, 480).opengl().resizable().build().unwrap();
    let gl_context = window.gl_create_context().unwrap();
    let gl = unsafe { glow::Context::from_loader_function(|s| video.gl_get_proc_address(s) as *const _) };
    let mut event_loop = sdl.event_pump().unwrap();

    // gl_attr.set_multisample_samples(4);

    let vertex_buffer = unsafe { gl.create_vertex_array().expect("uh nuh uh") };

    let test_shader = create_shader(&gl, "src/test.vert", "src/test.frag");

    let quad_shader = create_shader(&gl, "src/quad.vert", "src/quad.frag");
    let text_shader = create_shader(&gl, "src/quad.vert", "src/text.frag");

    let ft = Library::init().unwrap();

    let face = ft.new_face("res/fonts/ttf/JetBrainsMono-Regular.ttf", 0).unwrap();
    face.set_pixel_sizes(0, 15).unwrap();

    let mut glyphs = Vec::new();
    let mut max_advance = face.max_advance_height() >> 6;
    for i in 32..128 {
        face.load_char(i, LoadFlag::DEFAULT).unwrap();

        let glyph = face.glyph();
        glyph.render_glyph(freetype::RenderMode::Normal).unwrap();
        let bitmap = glyph.bitmap();

        unsafe {
            gl.pixel_store_i32(glow::UNPACK_ALIGNMENT, 1);
            let tex = gl.create_texture().unwrap();
            gl.bind_texture(glow::TEXTURE_2D, Some(tex));
            gl.tex_image_2d(glow::TEXTURE_2D,
                            0,
                            glow::R8 as i32,
                            bitmap.width(),
                            bitmap.rows(),
                            0,
                            glow::RED,
                            glow::UNSIGNED_BYTE,
                            Some(bitmap.buffer())
            );
            gl.tex_parameter_i32(glow::TEXTURE_2D, glow::TEXTURE_MAG_FILTER, glow::LINEAR as i32);
            gl.tex_parameter_i32(glow::TEXTURE_2D, glow::TEXTURE_MIN_FILTER, glow::LINEAR as i32);
            glyphs.push(Glyph {
                tex,
                bearing: (glyph.bitmap_left(), glyph.bitmap_top()),
                x: bitmap.width(),
                y: bitmap.rows(),
            });
        }
    }

    let font = LoadedFont {
        size: 15,
        glyphs,
        max_advance,
    };

    let drawing_context = DrawingContext {
        gl,
        video,
        quad_shader,
        text_shader,
        current_font: &font,
        window,
    };

    let black        = hex_code_to_color("282c34");
    let white        = hex_code_to_color("abb2bf");
    let light_red    = hex_code_to_color("e06c75");
    let dark_red     = hex_code_to_color("be5046");
    let green        = hex_code_to_color("98c379");
    let light_yellow = hex_code_to_color("e5c075");
    let dark_yellow  = hex_code_to_color("d19a66");
    let blue         = hex_code_to_color("61afef");
    let magenta      = hex_code_to_color("c678dd");
    let cyan         = hex_code_to_color("56b6c2");
    let gutter_gray  = hex_code_to_color("4b5263");
    let comment_gray = hex_code_to_color("5c6370");

    unsafe {
        drawing_context.gl.use_program(Some(text_shader));
        let background = srgb_to_rgb(black);
        drawing_context.gl.clear_color(background.0, background.1, background.2, 1.0);

        drawing_context.gl.enable(glow::FRAMEBUFFER_SRGB);
    }

    let mut quit = false;
    let mut pos = [0.0, 0.0];
    let mut vel = [0.4, 1.0];
    let mut old = std::time::Instant::now();

    drawing_context.video.text_input().start();
    drawing_context.video.text_input().stop();

    let mut editor = EditorState {
        filename: None,
        contents: None,
        cursor: (0, 0),
        mode: EditorMode::Normal,
        drawing_context: &drawing_context,
    };

    let fns = [EditorState::move_left, EditorState::move_right_normal, EditorState::move_down, EditorState::move_up];
    let mut commands: HashMap<&str, &dyn Fn(&mut EditorState)> = HashMap::new();
    commands.insert("move_left",  &| a | a.move_left());
    commands.insert("move_right", &| a | a.move_right_normal());
    commands.insert("move_up",    &| a | a.move_up());
    commands.insert("move_down",  &| a | a.move_down());
    commands.insert("open_below",  &| a | a.open_below());
    commands.insert("open_above",  &| a | a.open_above());
    commands.insert("insert",  &| a | a.insert_mode());
    commands.insert("insert_after",  &| a | a.insert_mode_after());
    commands.insert("move_word",  &| a | a.move_word());
    commands.insert("move_word_backwards",  &| a | a.move_word_backwards());
    commands.insert("backspace",  &| a | a.backspace());
    commands.insert("newline_at_cursor",  &| a | a.newline_at_cursor());
    commands.insert("normal_mode",  &| a | a.normal_mode());
    commands.insert("save_file",  &| a | a.save_file());

    let normal_keymap = parse_keymap(&commands, &[
        ("j", "move_left"),
        ("k", "move_down"),
        ("l", "move_up"),
        (";", "move_right"),
        ("u", "open_below"),
        ("U", "open_above"),
        ("i", "insert"),
        ("a", "insert_after"),
        ("w", "move_word"),
        ("t", "move_word_backwards"),
        ("spc d", "save_file")
    ]);

    let insert_keymap = parse_keymap(&commands, &[
        ("ret", "newline_at_cursor"),
        ("bsp", "backspace"),
        ("caps", "normal_mode"),
    ]);

    editor.load_file("fort.txt");

    let mut current_command = Vec::new();

    while !quit {
        let delta = old.elapsed().as_secs_f32();
        old = std::time::Instant::now();
        for event in event_loop.poll_iter() {
            match event {
                sdl2::event::Event::Quit { .. } => quit = true,
                Event::KeyDown { timestamp, window_id, keycode, scancode, keymod, repeat } => {
                    let key = Key {
                        scancode: scancode.unwrap(),
                        mods: keymod
                    };

                    current_command.push(key.clone());
                    println!("{:?}", current_command);
                    fn match_current_command(editor: &mut EditorState, keymap: &Vec<(Vec<Key>, &dyn Fn(&mut EditorState))>, current_command: &mut Vec<Key>) {
                        let mut found = false;
                        for m in keymap.iter() {
                            if m.0.len() >= current_command.len() && &m.0[..current_command.len()] == current_command {
                                found = true;
                            }
                            if &m.0 == current_command {
                                m.1(editor);
                                current_command.clear();
                            }
                        }
                        if !found {
                            current_command.clear();
                        }
                    }
                    match editor.mode {
                        EditorMode::Normal => {
                            match_current_command(&mut editor, &normal_keymap, &mut current_command);
                        }
                        EditorMode::Insert => {
                            match_current_command(&mut editor, &insert_keymap, &mut current_command);
                        }
                    }
                }
                Event::TextInput { timestamp, window_id, text } => {
                    if editor.mode == EditorMode::Insert {
                        editor.insert(&text);
                    }
                }
                Event::Window { win_event, .. } => {
                    match win_event {
                        WindowEvent::Resized(x, y) => {
                            unsafe { drawing_context.gl.viewport(0, 0, x, y) }
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        }

        pos[0] += delta * vel[0];
        pos[1] += delta * vel[1];

        for (i, v) in pos.iter_mut().enumerate() {
            if *v > 1.0 {
                vel[i] = -vel[i];
                *v = 1.0;
            }
            if *v < 0.0 {
                vel[i] = -vel[i];
                *v = 0.0;
            }
        }

        unsafe {
            drawing_context.gl.clear(glow::COLOR_BUFFER_BIT);

            let window_size = drawing_context.window.drawable_size();

            let block_size = (max_advance as i32 / 2 as i32, max_advance as i32);

            let cursor_size = (match editor.mode {
                EditorMode::Normal => { max_advance as i32 / 2 as i32 }
                EditorMode::Insert => { 2 }
            }, max_advance as i32);

            // println!("{}", max_advance);

            let pen_x = 5;

            drawing_context.gl.uniform_2_f32(drawing_context.gl.get_uniform_location(quad_shader, "offset").as_ref(), ((block_size.0 * editor.cursor.0 as i32 + pen_x as i32 * block_size.0) as f32 - 1.0) / window_size.0 as f32, (block_size.1 * editor.cursor.1) as f32 / window_size.1 as f32);
            drawing_context.gl.uniform_2_f32(drawing_context.gl.get_uniform_location(quad_shader, "scale").as_ref(), (cursor_size.0) as f32 / window_size.0 as f32, (cursor_size.1) as f32 / window_size.1 as f32);
            drawing_context.gl.uniform_4_f32(drawing_context.gl.get_uniform_location(quad_shader, "color").as_ref(), 0.38, 0.68, 0.93, 1.0);
            drawing_context.gl.draw_arrays(glow::TRIANGLES, 0, 6);

            drawing_context.gl.enable(glow::BLEND);
            drawing_context.gl.blend_func(glow::SRC_ALPHA, glow::ONE_MINUS_SRC_ALPHA);

            if let Some(ref contents) = editor.contents {
                for i in 0..(window_size.1 as i32 / block_size.1).min(contents.len() as i32) {
                    let (line_num, color) = if editor.cursor.1 - i == 0 { (editor.cursor.1 + 1, white) } else { (editor.cursor.1 - i, gutter_gray) };
                    let line_str = line_num.abs().to_string();
                    for (p, ch) in line_str.chars().rev().enumerate() {
                        drawing_context.draw_glyph_on_grid(ch, ((pen_x - 2) - p as i32, i as i32), color);
                    }
                }
            }

            // let mut pen = (0.5 * window_size.0 as f32, 0.5 * window_size.1 as f32);
            let mut pen = (pen_x, 0);

            let scale = 1.0; //15.0 / 64.0;

            if let Some(ref lines) = editor.contents {
                for line in lines {
                    for i in line.chars() {
                        drawing_context.draw_glyph_on_grid(i, pen, white);
                        pen.0 += 1;

                    }
                    pen.1 += 1;
                    // pen.1 += max_advance as f32;
                    pen.0 = pen_x;
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
