//#define _POSIX_C_SOURCE 200112L
#define WLR_USE_UNSTABLE

struct render_data {
  struct wlr_output *output;
  struct wlr_renderer *renderer;
  struct tinywl_view *view;
  struct timespec *when;
};

enum tinywl_cursor_mode {
	TINYWL_CURSOR_PASSTHROUGH,
	TINYWL_CURSOR_MOVE,
	TINYWL_CURSOR_RESIZE,
};

struct tinywl_server {
	struct wl_display *wl_display;
	struct wlr_backend *backend;
	struct wlr_renderer *renderer;

	struct wlr_xdg_shell *xdg_shell;
	struct wl_listener new_xdg_surface;
	struct wl_list views;

	struct wlr_cursor *cursor;
	struct wlr_xcursor_manager *cursor_mgr;
	struct wl_listener cursor_motion;
	struct wl_listener cursor_motion_absolute;
	struct wl_listener cursor_button;
	struct wl_listener cursor_axis;

	struct wlr_seat *seat;
	struct wl_listener new_input;
	struct wl_listener request_cursor;
	struct wl_list keyboards;
	enum tinywl_cursor_mode cursor_mode;
	struct tinywl_view *grabbed_view;
	double grab_x, grab_y;
	int grab_width, grab_height;
	uint32_t resize_edges;

	struct wlr_output_layout *output_layout;
	struct wl_list outputs;
	struct wl_listener new_output;
};

struct tinywl_output {
	struct wl_list link;
	struct tinywl_server *server;
	struct wlr_output *wlr_output;
	struct wl_listener frame;
};

struct tinywl_view {
	struct wl_list link;
	struct tinywl_server *server;
	struct wlr_xdg_surface *xdg_surface;
	struct wl_listener map;
	struct wl_listener unmap;
	struct wl_listener destroy;
	struct wl_listener request_move;
	struct wl_listener request_resize;
	bool mapped;
	int x, y;
};

struct tinywl_keyboard {
	struct wl_list link;
	struct tinywl_server *server;
	struct wlr_input_device *device;

	struct wl_listener modifiers;
	struct wl_listener key;
};

void focus_view(struct tinywl_view *view, struct wlr_surface *surface);
void keyboard_handle_modifiers(struct wl_listener *listener, void *data);
bool handle_keybinding(struct tinywl_server *server, xkb_keysym_t sym);
void keyboard_handle_key(struct wl_listener *listener, void *data);
void server_new_keyboard(struct tinywl_server *server, struct wlr_input_device *device);
void server_new_pointer(struct tinywl_server *server, struct wlr_input_device *device);
void server_new_input(struct wl_listener *listener, void *data);
void seat_request_cursor(struct wl_listener *listener, void *data);
bool view_at(struct tinywl_view *view, double lx, double ly, struct wlr_surface **surface, double *sx, double *sy);
struct tinywl_view *desktop_view_at(struct tinywl_server *server, double lx, double ly, struct wlr_surface **surface, double *sx, double *sy);
void process_cursor_move(struct tinywl_server *server, uint32_t time);
void process_cursor_resize(struct tinywl_server *server, uint32_t time);
void process_cursor_motion(struct tinywl_server *server, uint32_t time);
void server_cursor_motion(struct wl_listener *listener, void *data);
void server_cursor_motion_absolute(struct wl_listener *listener, void *data);
void server_cursor_button(struct wl_listener *listener, void *data);
void server_cursor_axis(struct wl_listener *listener, void *data);
void render_surface(struct wlr_surface *surface, int sx, int sy, void *data);
void output_frame(struct wl_listener *listener, void *data);
void server_new_output(struct wl_listener *listener, void *data);
void xdg_surface_map(struct wl_listener *listener, void *data);
void xdg_surface_unmap(struct wl_listener *listener, void *data);
void xdg_surface_destroy(struct wl_listener *listener, void *data);
void begin_interactive(struct tinywl_view *view, enum tinywl_cursor_mode mode, uint32_t edges);
void xdg_toplevel_request_move(struct wl_listener *listener, void *data);
void xdg_toplevel_request_resize(struct wl_listener *listener, void *data);
void server_new_xdg_surface(struct wl_listener *listener, void *data);
int tinywl();