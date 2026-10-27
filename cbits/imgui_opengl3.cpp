// ImGui's OpenGL 3 renderer, for trackkat's window (window/TrackKat/Window/GL.hs).
//
// dear-imgui's own wrapper calls glewInit first, which the renderer doesn't
// need (it loads OpenGL itself) but which would make trackkat.exe need
// glew32.dll. These call the renderer, compiled into dear-imgui, directly,
// declared as backends/imgui_impl_opengl3.h declares it.

struct ImDrawData;
bool ImGui_ImplOpenGL3_Init(const char *glsl_version);
void ImGui_ImplOpenGL3_NewFrame();
void ImGui_ImplOpenGL3_RenderDrawData(ImDrawData *draw_data);

extern "C" {

int trackkat_opengl3_init(void) { return ImGui_ImplOpenGL3_Init(nullptr); }

void trackkat_opengl3_new_frame(void) { ImGui_ImplOpenGL3_NewFrame(); }

void trackkat_opengl3_render(void *draw_data) { ImGui_ImplOpenGL3_RenderDrawData(static_cast<ImDrawData *>(draw_data)); }
}
