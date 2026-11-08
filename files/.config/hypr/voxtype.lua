-- Hold Super+V to dictate; release V to insert the transcription.
local dictationHeld = false
hl.bind("SUPER + V", function()
    dictationHeld = true
    hl.exec_cmd("voxtype record start")
end)
hl.bind("V", function()
    if dictationHeld then
        dictationHeld = false
        hl.exec_cmd("voxtype record stop")
    end
end, { release = true, ignore_mods = true, non_consuming = true })

-- Suppress desktop shortcuts while the transcription is being typed.
hl.define_submap("voxtype_output", function()
    for _, key in ipairs({"Super_L", "Super_R", "Control_L", "Control_R", "Alt_L", "Alt_R", "Shift_L", "Shift_R"}) do
        hl.bind(key, hl.dsp.exec_cmd("true"))
    end
    hl.bind("F12", hl.dsp.submap("reset"), { ignore_mods = true })
end)
