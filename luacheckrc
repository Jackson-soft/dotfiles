globals = {
	"Args",
	"Config",
	"Documents",
	"Globals",
	"Initialized",
	"Root",
	"Shutdown",
	"Types",
}
std="max"

files["spec/*.lua"].std = "+busted"
files["spec/*.rockspec"].std = "rockspec" 

files["rockspecs/*.rockspec"].std = "rockspec"
files["*.rockspec"].std = "rockspec" 

cache = true
