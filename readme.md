# Ondesked

Ondesked is a markup language intended to make GUI development of desktop applications much easier.

It's based on XML, and is similar to HTML. It transpiles to a C++, where a GUI toolkit is used to define the GUI. As of now, it uses wxWidgets as the GUI toolkit.

Ondesked is used for building the view of your application; it's more like the HTML for GUI development. The rest of the application can be written by extending the C++ code generated.

# Examples

## Ondesked Code

Here's some Ondesked code:

```xml
<app id="NumberIncrementor">
    <frame id="frame" class="centered visible" title="Number Generator">
        <box_sizer id="frame_sizer" class="vertical fit-parent">
            <panel id="panel" grow="1">
                <box_sizer class="vertical" id="panel_sizer">
                    <panel id="buttons_panel" grow="0" padding="xy-20">
                        <box_sizer id="buttons_panel_sizer" padding="xy-20" class="horizontal">
                            <button id="increment_button" padding="pr-20" onclick="increment">Increment</button>
                            <button id="decrement_button">Decrement</button>
                        </box_sizer>
                    </panel>
                    <textarea id="textarea" width="500" height="200" padding="xb-20" grow="1">Hey now brown cow!</textarea>
                </box_sizer>
            </panel>
        </box_sizer>
    </frame>
</app>
```

## Generated C++ Code

After compilation, code equivalent to this will be generated:

```cpp
bool NumberIncrementor::OnInit()
{
    wxFrame* frame = new wxFrame(NULL, wxID_ANY, wxT("Number Generator"), wxDefaultPosition, wxDefaultSize);

    frame->Center();
    frame->Show(true);

    wxBoxSizer* frame_sizer = new wxBoxSizer(wxVERTICAL);

    frame->SetSizer(frame_sizer);
    wxPanel* panel = new wxPanel(frame);

    wxBoxSizer* panel_sizer = new wxBoxSizer(wxVERTICAL);

    panel->SetSizer(panel_sizer);
    wxPanel* buttons_panel = new wxPanel(panel);

    wxBoxSizer* buttons_panel_sizer = new wxBoxSizer(wxHORIZONTAL);

    buttons_panel->SetSizer(buttons_panel_sizer);
    wxButton* increment_button = new wxButton(buttons_panel, wxID_ANY, wxT("Increment"));

    wxButton* decrement_button = new wxButton(buttons_panel, wxID_ANY, wxT("Decrement"));

    buttons_panel_sizer->Add(increment_button, 0, wxEXPAND | wxRIGHT, 20);
    buttons_panel_sizer->Add(decrement_button, 0, wxEXPAND | wxLEFT | wxRIGHT | wxTOP | wxBOTTOM, 0);

    wxTextCtrl* textarea = new wxTextCtrl(panel, wxID_ANY, wxT("Hey now brown cow!"), wxDefaultPosition, wxSize(500, 200), wxTE_MULTILINE);

    panel_sizer->Add(buttons_panel, 0, wxEXPAND | wxLEFT | wxRIGHT | wxTOP | wxBOTTOM, 20);
    panel_sizer->Add(textarea, 1, wxEXPAND | wxLEFT | wxRIGHT | wxBOTTOM, 20);

    frame_sizer->Add(panel, 1, wxEXPAND | wxLEFT | wxRIGHT | wxTOP | wxBOTTOM, 0);
    frame->GetSizer()->Fit(frame);

    return true;
}
```

## Result

This is how the desktop application may look like after compiled:

<img src="https://raw.githubusercontent.com/nahiyan/ondesked/master/docs/images/result.png"/>