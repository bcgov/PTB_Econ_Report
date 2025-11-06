-- set_output_name.lua
-- Quarto/Pandoc Lua filter to dynamically set output filename and directory
-- Pattern: results/report_<USERNAME>_<YYYYMMDD_HHMMSS>

local function get_username()
  return os.getenv('USERNAME') or os.getenv('USER') or 'user'
end

local function timestamp()
  return os.date('%Y%m%d_%H%M%S')
end

function Meta(meta)
  local user = get_username()
  local ts = timestamp()
  local base = string.format('report_%s_%s', user, ts)

  -- set output-file (format extension added by Quarto/Pandoc)
  meta["output-file"] = pandoc.MetaString(base)

  -- ensure output-dir is results
  meta["output-dir"] = pandoc.MetaString('results')

  return meta
end

