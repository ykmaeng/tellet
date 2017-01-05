defmodule Tellet.Services.Telephony do

  use Tellet.Service

  defun say args, state do
    next! state
  end

end

