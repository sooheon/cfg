function fish_user_key_bindings
  if test "$TERMINAL_EMULATOR" = "JetBrains-JediTerm"
    bind ƒ forward-word
    bind ∫ backward-word
    bind ∂ delete-word
  end
  bind \co lf
end
