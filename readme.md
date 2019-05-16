# Ondesked

Ondesked is a markup language intended to make desktop application GUI development easier.

It's based on XML, and is similar to HTML. It transpiles to a C++, where a GUI toolkit is used to define the GUI. As of now, it uses wxWidgets as the GUI toolkit.

Ondesked is used for building the view of your application; it's more like the HTML for GUI development. The rest of the application can be written by extending the C++ code generated.

# Examples

## Ondesked Code

Here's some Ondesked code:

```xml
<app id="NumberIncrementor">
    <frame class="centered visible" title="Number Generator">
        <box_sizer class="vertical fit-parent">
            <panel>
                <box_sizer class="vertical">
                    <panel padding="20 20 20 20" grow="0">
                        <box_sizer class="horizontal">
                            <button padding="0 20 0 0" onclick="increment">Increment</button>
                            <button>Decrement</button>
                        </box_sizer>
                    </panel>
                    <textarea width="200" height="100" padding="0 20 20 20" grow="1">Hey now brown cow!</textarea>
                </box_sizer>
            </panel>
        </box_sizer>
    </frame>
</app>
```

## Generated C++ Code

After compilation, code equivalent to this maybe generated:

```cpp
bool MyApp::OnInit()
{
    // frame

    wxFrame* frame = new wxFrame(NULL, wxID_ANY, wxT("Hello World!"), wxDefaultPosition, wxDefaultSize);

    // frame - center and show

    frame->Centre();
    frame->Show(true);

    // frame sizer

    wxBoxSizer* frame_sizer = new wxBoxSizer(wxVERTICAL);
    frame->SetSizer(frame_sizer);

    // panel

    wxPanel* panel = new wxPanel(frame);
    frame_sizer->Add(panel, 1, wxEXPAND, 0);

    // horizontal panel

    wxPanel* buttons_panel = new wxPanel(panel);

    // increment button

    wxButton* increment = new wxButton(buttons_panel, wxID_ANY, wxT("Increment"));

    // decrement button

    wxButton* decrement = new wxButton(buttons_panel, wxID_ANY, wxT("Decrement"));

    // textarea

    this->textarea = new wxTextCtrl(panel, wxID_ANY, wxT("Hey"), wxDefaultPosition, wxSize(500, 300), wxTE_MULTILINE);

    this->textarea->SetValue(wxT("Hey now brown cow!"));

    // vertical sizer for the components

    wxBoxSizer* panel_sizer = new wxBoxSizer(wxVERTICAL);
    panel_sizer->Add(buttons_panel, 0, wxALL, 20);
    panel_sizer->Add(this->textarea, 1, wxEXPAND | wxLEFT | wxRIGHT | wxBOTTOM, 20);
    panel->SetSizer(panel_sizer);

    // horizontal sizer for the buttons

    wxBoxSizer* buttons_panel_sizer = new wxBoxSizer(wxHORIZONTAL);
    buttons_panel_sizer->Add(increment, 0, wxEXPAND | wxRIGHT, 20);
    buttons_panel_sizer->Add(decrement, 0, wxEXPAND, 0);

    // buttons panel - set sizer

    buttons_panel->SetSizer(buttons_panel_sizer);

    // size the frame

    frame->GetSizer()->Fit(frame);

    // bind button events

    increment->Bind(wxEVT_BUTTON, &MyApp::Increment, this);

    return true;
}
```

## Results

This is how the desktop application may look like after compiled:

<img src="https://github.com/nahiyan/ondesked/blob/master/docs/images/result.png"/>