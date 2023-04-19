function fish_mode_prompt
  switch $fish_bind_mode
    case default
      set_color --bold red
      echo '≎'
    case insert
      set_color --bold green
      echo '∮'
    case replace_one
      set_color --bold green
      echo '∯'
    case visual
      set_color --bold brmagenta
      echo ''
    case '*'
      set_color --bold red
      echo ''
  end
  set_color normal
end
