-module(grapher).
-compile(export_all).

-include_lib("wx/include/wx.hrl").

-define(ABOUT,?wxID_ABOUT).
-define(EXIT,?wxID_EXIT).

start() ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "MicroBlog"),
    setup(Frame),
    wxFrame:show(Frame),
    loop(Frame),
    wx:destroy().

setup(Frame) ->
    MenuBar = wxMenuBar:new(),
    File = wxMenu:new(),
    Help = wxMenu:new(),
    wxMenu:append(Help,?ABOUT,"About MicroBlog"),
    wxMenu:append(File,?EXIT,"Quit"),
    wxMenuBar:append(MenuBar,File,"&File"),
    wxMenuBar:append(MenuBar,Help,"&Help"),
    wxFrame:setMenuBar(Frame,MenuBar),
    wxFrame:createStatusBar(Frame),
    wxFrame:setStatusText(Frame,"Welcome to wxErlang"),
    wxFrame:connect(Frame, command_menu_selected),
    wxFrame:connect(Frame, close_window),
    wxEvtHandler:connect(Frame,left_up).

loop(Frame) ->
    receive
	#wx{id=?ABOUT, event=#wxCommand{}} ->
	    Str = "MicroBlog is a minimal WxErlang example.",
	    MD = wxMessageDialog:new(Frame,Str,
				     [{style, ?wxOK bor ?wxICON_INFORMATION},
				      {caption, "About MicroBlog"}]),
	    wxDialog:showModal(MD),
	    wxDialog:destroy(MD),
	    loop(Frame);
	#wx{id=?EXIT, event=#wxCommand{type=command_menu_selected}} ->
	    wxWindow:close(Frame,[]);
	#wx{event=#wxMouse{type=left_up,x=X,controlDown=true}}->
		    io:write(X),
		    loop(Frame)
		
    end.
