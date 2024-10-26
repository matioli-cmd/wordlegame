// WORDS //

let wordleWords = [
    "aback", "abase", "abate", "abbey", "abbot", "abhor", "abide", "abled", "abode", "abort", "about", 
    "above", "abuse", "abyss", "acorn", "acrid", "actor", "acute", "adage", "adapt", "adept", "admin", 
    "admit", "adobe", "adopt", "adore", "adorn", "adult", "affix", "afire", "afoot", "after", "again", 
    "agape", "agate", "agent", "agile", "aging", "aglow", "agony", "agree", "ahead", "aider", "aisle", 
    "alarm", "album", "alert", "algae", "alibi", "alien", "align", "alike", "alive", "allay", "alley", 
    "allot", "allow", "alloy", "aloft", "alone", "along", "aloof", "aloud", "alpha", "altar", "alter", 
    "amass", "amaze", "amber", "amble", "amend", "amiss", "amity", "among", "ample", "amply", "amuse", 
    "angel", "anger", "angle", "angry", "angst", "anime", "ankle", "annex", "annoy", "annul", "anode", 
    "antic", "anvil", "aorta", "apart", "aphid", "apple", "apply", "apron", "aptly", "arbor", "arced", 
    "ardor", "arena", "argon", "argue", "arise", "armed", "armor", "aroma", "arose", "array", "arrow", 
    "arson", "artsy", "ascot", "ashen", "aside", "asked", "asker", "askew", "aspen", "aspic", "assay", 
    "asset", "atlas", "atoll", "atone", "attic", "audio", "audit", "augur", "aunty", "aural", "avail", 
    "avian", "avoid", "await", "awake", "award", "aware", "awash", "awful", "awoke", "axial", "axiom", 
    "axion", "azure", "bacon", "badge", "badly", "bagel", "baggy", "baker", "baler", "balmy", "banal", 
    "banjo", "barge", "baron", "basal", "basic", "basil", "basin", "basis", "baste", "batch", "bathe", 
    "baton", "batty", "bawdy", "bayou", "beach", "beady", "beard", "beast", "beech", "beefy", "befit", 
    "began", "begat", "beget", "begin", "begun", "being", "belch", "belie", "belle", "belly", "below", 
    "bench", "beret", "berry", "berth", "beset", "betel", "bevel", "bezel", "bible", "bicep", "biddy", 
    "bigot", "bilge", "billy", "binge", "bingo", "biome", "birch", "birth", "bison", "bitch", "bitty", 
    "black", "blade", "blame", "bland", "blank", "blare", "blast", "blaze", "bleak", "bleat", "bleed", 
    "bleep", "blend", "bless", "blimp", "blind", "blink", "bliss", "blitz", "bloat", "block", "bloke", 
    "blond", "blood", "bloom", "blown", "bluer", "bluff", "blunt", "blurb", "blurt", "blush", "board", 
    "boast", "bobby", "boney", "bongo", "bonus", "booby", "boost", "booth", "booty", "booze", "boozy", 
    "borax", "borne", "botch", "bough", "bound", "bowel", "boxer", "brace", "braid", "brain", "brake", 
    "brand", "brash", "brass", "brave", "bravo", "brawl", "brawn", "bread", "break", "breed", "briar", 
    "bribe", "brick", "bride", "brief", "brine", "bring", "brink", "briny", "brisk", "broad", "broil", 
    "broke", "brood", "brook", "broom", "broth", "brown", "brunt", "brush", "brute", "buddy", "budge", 
    "buggy", "bugle", "build", "built", "bulge", "bulky", "bully", "bumpy", "bunch", "bunny", "burly", 
    "burnt", "burro", "burst", "bushy", "butch", "butte", "buxom", "buyer", "bylaw", "cabin", "cable", 
    "cacao", "cache", "cacti", "caddy", "cadet", "cagey", "cairn", "camel", "cameo", "canal", "candy", 
    "canny", "canoe", "canon", "caper", "carat", "cargo", "carol", "carry", "carve", "caste", "catch", 
    "cater", "catty", "caulk", "cause", "cavil", "cease", "cedar", "cello", "chafe", "chaff", "chain", 
    "chair", "chalk", "champ", "chant", "chaos", "charm", "chart", "chase", "chasm", "cheap", "cheat", 
    "check", "cheek", "cheer", "chess", "chest", "chick", "chide", "chief", "child", "chili", "chill", 
    "chime", "china", "chirp", "chock", "choir", "choke", "chomp", "chord", "chore", "chose", "chuck", 
    "chump", "chunk", "churn", "chute", "cider", "cigar", "cinch", "circa", "civic", "civil", "clack", 
    "claim", "clamp", "clang", "clank", "clash", "clasp", "class", "clean", "clear", "cleat", "cleft", 
    "clerk", "click", "cliff", "climb", "cling", "clink", "cloak", "clock", "clone", "close", "cloth", 
    "cloud", "clout", "clove", "clown", "cluck", "clued", "clump", "clung", "coach", "coast", "cobra", 
    "cocoa", "colon", "color", "comet", "comfy", "comic", "comma", "conch", "condo", "conic", "copse", 
    "coral", "corer", "corny", "couch", "cough", "could", "count", "coupe", "court", "coven", "cover", 
    "covet", "covey", "cower", "crack", "craft", "cramp", "crane", "crank", "crash", "crass", "crate", 
    "crave", "crawl", "craze", "crazy", "creak", "cream", "credo", "creek", "creep", "creme", "crepe", 
    "crept", "cress", "crest", "crick", "cried", "crier", "crime", "crimp", "crisp", "croak", "crone", 
    "crony", "crook", "cross", "croup", "crowd", "crown", "crude", "cruel", "crumb", "crush", "crust", 
    "crypt", "cubic", "cumin", "curio", "curly", "curse", "curve", "curvy", "cutie", "cyber", "cycle", 
    "cynic", "daddy", "daily", "dairy", "daisy", "dance", "dandy", "datum", "daunt", "dealt", "death", 
    "debar", "debit", "debug", "debut", "decaf", "decal", "decay", "decor", "decoy", "decry", "defer", 
    "deign", "deity", "delay", "delta", "delve", "demon", "demur", "denim", "dense", "depot", "depth", 
    "derby", "deter", "detox", "deuce", "devil", "diary", "dicey", "digit", "dilly", "dimly", "dingo", 
    "dingy", "diode", "dirge", "dirty", "disco", "ditch", "ditto", "ditty", "diver", "dizzy", "dodge", 
    "dogma", "doing", "dolly", "donor", "donut", "dopey", "doubt", "dough", "dowdy", "dowel", "downy", 
    "dowry", "dozen", "draft", "drain", "drake", "drama", "drank", "drape", "drawl", "drawn", "dread", 
    "dream", "dress", "dried", "drier", "drift", "drill", "drink", "drone", "drool", "droop", "drove", 
    "drown", "druid", "drunk", "dryer", "dryly", "duchy", "dummy", "dumpy", "dunce", "dusky", "dusty", 
    "dutch", "duvet", "dwarf", "dwell", "dwelt", "dying", "eager", "eagle", "early", "earth", "easel", 
    "eaten", "eater", "ebony", "eclat", "edict", "edify", "eerie", "egret", "eight", "eject", "eking", 
    "elate", "elbow", "elder", "elect", "elegy", "elfin", "elide", "elite", "elope", "elude", "email", 
    "embed", "ember", "emcee", "empty", "enact", "endow", "enema", "enemy", "enjoy", "ennui", "ensue", 
    "enter", "entry", "envoy", "epoch", "epoxy", "equal", "equip", "erase", "erect", "erode", "error", 
    "erupt", "essay", "ester", "ether", "ethic", "ethos", "etude", "evade", "event", "every", "evict", 
    "exact", "exalt", "excel", "exert", "exile", "exist", "expel", "extol", "extra", "exult", "eying", 
    "fable", "facet", "faint", "fairy", "faith", "false", "fancy", "fanny", "farce", "fatal", "fatty", 
    "fault", "fauna", "favor", "feast", "fecal", "feign", "fella", "felon", "femme", "femur", "fence", 
    "fever", "fewer", "fiber", "fibre", "ficus", "field", "fiend", "fiery", "fifth", "fifty", "fight", 
    "filer", "filet", "filly", "filmy", "filth", "final", "finch", "finis", "fired", "firer", "firry", 
    "first", "fishy", "fixer", "fizzy", "flack", "flail", "flair", "flake", "flaky", "flame", "flank", 
    "flare", "flash", "flask", "fleck", "fleet", "flesh", "flick", "flier", "fling", "flint", "flirt", 
    "float", "flock", "flood", "floor", "flora", "floss", "flour", "flout", "flown", "fluff", "fluid", 
    "fluke", "flume", "flung", "flunk", "flush", "flute", "flyer", "foamy", "focal", "focus", "foggy", 
    "foist", "folio", "folly", "foray", "force", "forge", "forgo", "forte", "forth", "forty", "forum", 
    "found", "foyer", "frail", "frame", "frank", "fraud", "freak", "freed", "freer", "fresh", "friar", 
    "fried", "frier", "frill", "frisk", "fritz", "frock", "frond", "front", "frost", "froth", "frown", 
    "froze", "fruit", "fudge", "fugue", "fully", "fungi", "funky", "funny", "furor", "furry", "fussy", 
    "fuzzy", "gaffe", "gaily", "gamer", "gamma", "gamut", "gassy", "gaudy", "gauge", "gaunt", "gauze", 
    "gavel", "gawky", "gayer", "gayly", "gazer", "gecko", "geeky", "geese", "genie", "genre", "ghost", 
    "ghoul", "giant", "giddy", "gipsy", "girly", "girth", "given", "giver", "glade", "gland", "glare", 
    "glass", "glaze", "gleam", "glean", "glide", "glint", "gloat", "globe", "gloom", "glory", "gloss", 
    "glove", "glued", "gluer", "gnash", "gnome", "godly", "going", "golem", "golly", "goner", "goody", 
    "gooey", "goofy", "goose", "gorge", "gouge", "gourd", "grace", "grade", "graft", "grail", "grain", 
    "grand", "grant", "grape", "graph", "grasp", "grass", "grate", "grave", "gravy", "graze", "great", 
    "greed", "green", "greet", "grief", "grill", "grime", "grimy", "grind", "gripe", "groan", "groin", 
    "groom", "grope", "gross", "group", "grout", "grove", "growl", "grown", "gruel", "gruff", "grunt", 
    "guard", "guava", "guess", "guest", "guide", "guild", "guile", "guilt", "guise", "gulch", "gully", 
    "gumbo", "gummy", "guppy", "gusto", "gusty", "gypsy", "habit", "hairy", "halve", "handy", "happy", 
    "hardy", "harem", "harpy", "harry", "harsh", "haste", "hasty", "hatch", "hater", "haunt", "haute", 
    "haven", "havoc", "hazel", "heady", "heard", "heart", "heath", "heave", "heavy", "hedge", "hefty", 
    "heist", "helix", "hello", "hence", "heron", "hilly", "hinge", "hippo", "hippy", "hitch", "hoard", 
    "hobby", "hoist", "holly", "homer", "honey", "honor", "horde", "horny", "horse", "hotel", "hound", 
    "house", "hovel", "hover", "howdy", "human", "humid", "humor", "humph", "humus", "hunch", "hunky", 
    "hurry", "husky", "hussy", "hutch", "hydro", "hyena", "hymen", "hyper", "icily", "icing", "ideal", 
    "idiom", "idiot", "idler", "idyll", "igloo", "image", "imbue", "impel", "imply", "inbox", "incur", 
    "index", "inept", "inert", "infer", "ingot", "inlay", "inlet", "inner", "input", "inter", "intro", 
    "ionic", "irate", "irony", "islet", "issue", "itchy", "ivory", "jaunt", "jazzy", "jelly", "jerky", 
    "jetty", "jewel", "jiffy", "joint", "joist", "joker", "jolly", "joust", "judge", "juice", "juicy", 
    "jumbo", "jumpy", "junta", "junto", "juror", "kappa", "karma", "kayak", "kebab", "ketch", "keyed", 
    "khaki", "kinky", "kitty", "knack", "knave", "knead", "kneed", "kneel", "knelt", "knife", "knock", 
    "knoll", "known", "koala", "krill", "label", "labor", "lacey", "laddy", "laden", "ladle", "lager", 
    "lance", "lanky", "lapel", "lapse", "large", "larva", "lasso", "latch", "later", "lathe", "latte", 
    "laugh", "layer", "leach", "leafy", "leaky", "leant", "leapt", "learn", "lease", "leash", "least", 
    "leave", "ledge", "leech", "leery", "lefty", "legal", "leggy", "lemon", "lemur", "leper", "level", 
    "lever", "libel", "liege", "light", "liken", "lilac", "limbo", "limit", "linen", "liner", "lingo", 
    "lipid", "lithe", "liver", "livid", "llama", "loamy", "loath", "lobby", "local", "locus", "lodge", 
    "lofty", "logic", "login", "loopy", "loose", "lorry", "loser", "louse", "lousy", "lover", "lower", 
    "lowly", "loyal", "lucid", "lucky", "lumen", "lumpy", "lunar", "lunch", "lunge", "lupus", "lurch", 
    "lurid", "lusty", "lying", "lymph", "lyric", "macaw", "macho", "macro", "madam", "madly", "mafia", 
    "magic", "magma", "maize", "major", "maker", "mambo", "mamma", "mammy", "manga", "mange", "mango", 
    "mangy", "manic", "manly", "manor", "maple", "march", "marry", "marsh", "mason", "masse", "match", 
    "matey", "mauve", "maxim", "maybe", "mayor", "mealy", "meant", "meaty", "medal", "media", "medic", 
    "melee", "melon", "mercy", "merge", "merit", "merry", "messy", "metal", "meter", "metro", "micro", 
    "midge", "midst", "might", "milky", "mimic", "mince", "miner", "minim", "minor", "minty", "minus", 
    "mirth", "miser", "missy", "mocha", "modal", "model", "modem", "mogul", "moist", "molar", "moldy", 
    "money", "month", "moody", "moose", "moral", "moron", "morph", "mossy", "motel", "motif", "motor", 
    "motto", "moult", "mound", "mount", "mourn", "mouse", "mousy", "mouth", "mover", "movie", "mower", 
    "mucky", "mucus", "muddy", "mulch", "mummy", "munch", "mural", "murky", "mushy", "music", "musky", 
    "musty", "myrrh", "nadir", "naive", "nanny", "nasal", "nasty", "natal", "naval", "navel", "needy", 
    "neigh", "nerdy", "nerve", "never", "newer", "newly", "nicer", "niche", "niece", "night", "ninja", 
    "ninny", "ninth", "noble", "nobly", "noise", "noisy", "nomad", "noose", "north", "nosey", "notch", 
    "novel", "nudge", "nurse", "nutty", "nylon", "nymph", "oaken", "obese", "occur", "ocean", "octal", 
    "octet", "oddly", "offal", "offer", "often", "olden", "older", "olive", "ombre", "omega", "onion", 
    "onset", "opera", "opine", "opium", "optic", "orbit", "order", "organ", "other", "otter", "ought", 
    "ounce", "outdo", "outer", "outgo", "ovary", "ovate", "overt", "ovine", "ovoid", "owing", "owner", 
    "oxide", "ozone", "paddy", "pagan", "paint", "paler", "palsy", "panel", "panic", "pansy", "papal", 
    "paper", "pappy", "parch", "parer", "parka", "parry", "parse", "party", "pasta", "pasty", "patch", 
    "patio", "patsy", "patty", "pause", "payee", "payer", "peace", "peach", "pearl", "pecan", "pedal", 
    "penal", "pence", "penne", "penny", "perch", "peril", "perky", "perry", "pesky", "pesto", "petal", 
    "petty", "piano", "picky", "piece", "piety", "piggy", "pilaf", "piled", "piler", "pilot", "pinch", 
    "piney", "pinky", "pinto", "piper", "pique", "pitch", "pithy", "pivot", "pixel", "pixie", "pizza", 
    "place", "plaid", "plain", "plait", "plane", "plank", "plant", "plate", "plaza", "plead", "pleat", 
    "plied", "plier", "pluck", "plumb", "plume", "plump", "plunk", "plush", "poesy", "point", "poise", 
    "poker", "polar", "polka", "polyp", "pooch", "poppy", "porch", "poser", "posit", "posse", "pouch", 
    "pound", "pouty", "power", "preen", "press", "price", "prick", "pride", "pried", "prime", "primo", 
    "print", "prior", "prism", "privy", "prize", "probe", "prone", "prong", "proof", "prose", "proud", 
    "prove", "prowl", "proxy", "prude", "prune", "psalm", "pubic", "pudgy", "puffy", "pulpy", "pulse", 
    "punch", "pupil", "puppy", "puree", "purer", "purge", "purse", "pushy", "putty", "quack", "quail", 
    "quake", "qualm", "quark", "quart", "quash", "quasi", "queen", "queer", "quell", "query", "quest", 
    "queue", "quick", "quiet", "quill", "quilt", "quirk", "quite", "quota", "quote", "quoth", "rabbi", 
    "rabid", "racer", "radar", "radii", "radio", "rainy", "raise", "rajah", "rally", "ralph", "ramen", 
    "ranch", "randy", "range", "rapid", "rarer", "raspy", "ratio", "ratty", "raven", "rayon", "razor", 
    "reach", "react", "ready", "realm", "rearm", "rebar", "rebel", "rebus", "rebut", "recap", "recur", 
    "recut", "reedy", "refer", "refit", "regal", "rehab", "reign", "relax", "relay", "relic", "remit", 
    "renal", "renew", "repay", "repel", "reply", "rerun", "reset", "resin", "retch", "retro", "retry", 
    "reuse", "revel", "revue", "rhino", "rhyme", "rider", "ridge", "rifle", "right", "rigid", "rigor", 
    "rinse", "ripen", "riper", "risen", "riser", "risky", "rival", "river", "rivet", "roach", "roast", 
    "robin", "robot", "rocky", "rodeo", "roger", "rogue", "roomy", "roost", "rotor", "rouge", "rough", 
    "round", "rouse", "route", "rover", "rowdy", "rower", "royal", "ruddy", "ruder", "rugby", "ruler", 
    "rumba", "rumor", "rupee", "rural", "rusty", "sadly", "safer", "saint", "salad", "salon", "salsa", 
    "salty", "salve", "salvo", "sandy", "saner", "sappy", "sassy", "satin", "satyr", "sauce", "saucy", 
    "sauna", "saute", "saved", "saver", "savvy", "scald", "scale", "scalp", "scamp", "scant", "scare", 
    "scarf", "scary", "scene", "scent", "scion", "scoff", "scold", "scone", "scoop", "scope", "score", 
    "scorn", "scout", "scowl", "scram", "scrap", "scree", "screw", "scrub", "scrum", "scuba", "sedan", 
    "seedy", "segue", "seize", "semen", "sense", "sepia", "serif", "serum", "serve", "setup", "seven", 
    "sever", "sewed", "sewer", "shack", "shade", "shady", "shaft", "shake", "shaky", "shale", "shall", 
    "shalt", "shame", "shank", "shape", "shard", "share", "shark", "sharp", "shave", "shawl", "shear", 
    "sheen", "sheep", "sheer", "sheet", "sheik", "shelf", "shell", "shewn", "shift", "shine", "shiny", 
    "shire", "shirk", "shirt", "shoal", "shock", "shone", "shook", "shoot", "shore", "shorn", "short", 
    "shout", "shove", "shown", "showy", "shrew", "shrub", "shrug", "shuck", "shunt", "shush", "shyly", 
    "siege", "sieve", "sight", "sigma", "silky", "silly", "since", "sinew", "singe", "siren", "sissy", 
    "sixth", "sixty", "skate", "skein", "skimp", "skirt", "slack", "slain", "slang", "slant", "slash", 
    "slate", "sauce", "slaty", "slave", "slack", "slide", "slime", "sling", "slung", "smack", "smile", 
    "smith", "smock", "smoke", "smoky", "smote", "snack", "snail", "snake", "snaky", "snare", "sneak", 
    "sneer", "snide", "sniff", "snipe", "snore", "snout", "snowy", "speak", "speed", "spell", "spelt", 
    "spend", "spent", "spew", "spice", "spicy", "spill", "spilt", "spine", "spiny", "split", "spoil", 
    "spoke", "sponge", "spooky", "spore", "sport", "spotty", "spout", "spray", "spree", "squab", "squad", 
    "squat", "stain", "stale", "stalk", "stall", "stamp", "stand", "stare", "stark", "start", "stash", 
    "state", "stave", "stew", "stiff", "still", "sting", "stink", "stock", "stole", "stomp", "stone", 
    "stool", "stony", "stork", "storm", "story", "stove", "straw", "stray", "strip", "strop", "stuck", 
    "study", "stung", "style", "styro", "suave", "sugar", "suite", "sunny", "super", "surer", "swarm", 
    "swash", "swath", "swear", "sweat", "sweep", "sweet", "swell", "swept", "swift", "swine", "swing", 
    "swore", "sworn", "sword", "swung", "sync", "table", "taffy", "taint", "tally", "talon", "tamer", 
    "tango", "tangy", "taper", "tapir", "tardy", "tarry", "taste", "tasty", "tatty", "taunt", "teach", 
    "tease", "tempo", "tenet", "tenor", "tenth", "tepee", "thank", "tharp", "their", "thick", "thief", 
    "thigh", "thing", "think", "third", "thong", "thorn", "those", "threw", "throb", "throw", "thump", 
    "thyme", "tibia", "tidal", "tiger", "tight", "timid", "timer", "tired", "titan", "title", "toast", 
    "today", "token", "tonic", "tooth", "torch", "total", "touch", "tough", "towel", "tower", "toxic", 
    "toxin", "trace", "track", "tract", "trade", "trail", "train", "trait", "tramp", "trash", "tread", 
    "treat", "trend", "triad", "trial", "tribe", "trick", "tried", "tripe", "trite", "troll", "troop", 
    "trophy", "truck", "truly", "trunk", "trust", "truth", "twice", "twine", "twirl", "twist", "ultra", 
    "unbar", "uncle", "under", "union", "unite", "unity", "untie", "until", "upend", "upper", "upset", 
    "urban", "urine", "usage", "usher", "usual", "usurp", "uteri", "utile", "utter", "vague", "valet", 
    "valid", "valor", "value", "valve", "vapid", "vapor", "vault", "vegan", "venom", "venue", "verge", 
    "verse", "verve", "vicar", "video", "vigil", "vigor", "villa", "vinyl", "viola", "viper", "viral", 
    "virus", "visit", "visor", "vista", "vivid", "vocal", "vodka", "vogue", "voice", "voter", "vouch", 
    "vowel", "wacky", "wafer", "wager", "wagon", "waist", "waive", "waltz", "warty", "watch", "water", 
    "waver", "waxen", "weary", "weave", "wedge", "weedy", "weigh", "weird", "welch", "welsh", "whack", 
    "whale", "wharf", "wheat", "wheel", "whelp", "where", "which", "whiff", "while", "whine", "whirl", 
    "whisk", "white", "whole", "whoop", "whose", "widen", "wider", "widow", "width", "wield", "wight", 
    "wince", "winch", "windy", "wiser", "wispy", "witch", "witty", "woken", "woman", "women", "woody", 
    "woozy", "wordy", "world", "worry", "worse", "worst", "worth", "would", "wound", "woven", "wrack", 
    "wrapt", "wrath", "wreak", "wreck", "wring", "wrist", "write", "wrong", "wrote", "wrung", "wryly", 
    "xenon", "yacht", "yearn", "yeast", "yield", "young", "youth", "zebra", "zonal", "zooms",
    "zappy", "zebra", "zesty", "zilch", "zincs", "zingy", "zippy", "zonal", "zones", "zooms", "zappy", 
    "zappy", "zazen", "zebec", "zebus", "zeros", "ziffs", "zincs", "zines", "zings", "zippy", "zircs", 
    "zitis", "zloty", "zoeae", "zoeal", "zombi", "zonae", "zoned", "zoner", "zowie"
      
      ]
  

// OTHER VARIABLES // 

let original_green = 'rgb(67, 173, 61)'
let original_yellow = 'rgb(235, 206, 65)'

let contrast_green = 'rgb(191, 57, 33)'
let contrast_yellow = 'rgb(33, 146, 191)'

let correct = 'rgb(67, 173, 61)'
let inside = 'rgb(235, 206, 65)'
let none = 'rgb(67, 67, 66)'

// ROWS //

const First_Row = document.getElementById('1')
const Second_Row = document.getElementById('2')
const Third_Row = document.getElementById('3')
const Fourth_Row = document.getElementById('4')
const Fifth_Row = document.getElementById('5')
const Sixth_Row = document.getElementById('6')

// COnTRAST VARIABLES // 

const contrast_button = document.getElementById("enablecontrast")
const contrast_header = document.getElementById("contrast")

// MODE VARIABLES //

const headertext = document.getElementById("headertext")
const boxes = document.querySelectorAll(".row h1")
const background = document.getElementById("background")
const settingsheading = document.getElementById("heading")
const settingsmode = document.getElementById("mode")
const settingsline = document.getElementById("line")
const settingsbackground = document.getElementById("settings")
const settings_image = document.getElementById("settingsimage")
const light_image = document.getElementById("settingsimagelight")
const dark_image = document.getElementById("settingsimagedark")

let mode = 'darkmode'
let SettingsOpened = 'N'
let listening = true

// SETTINGS //

let settings_button = document.getElementById("settingsbutton")
let settings_gui = document.getElementById("settings")
let close_button = document.getElementById("close")
let enable = document.getElementById("enable")

settings_button.onclick = function(){
    
    settings_gui.style.display = 'flex'
    SettingsOpened = 'Y'
    settings_button.style.visibility = 'hidden'
    if(mode == 'darkmode'){
        dark_image.style.visibility = 'hidden'
    }
    else{
        light_image.style.visibility = 'hidden'
    }
    errorMessage.textContent = ''
    
    close_button.onclick = function(){
        settings_button.style.visibility = 'visible'
        settings_gui.style.display = 'none'
        SettingsOpened = 'N'
        if(mode == 'darkmode'){
            dark_image.style.visibility = 'visible'
        }
        else{
            light_image.style.visibility = 'visible'
        }
    }

    enable.onclick = function(){
        if(enable.textContent == 'Disable'){

            // SWITCH COLORS //

            enable.textContent = 'Enable'
            enable.style.backgroundColor = 'rgb(50, 177, 65)'
            
            enable.addEventListener('mouseover', () => {
                enable.style.backgroundColor = 'rgb(34, 120, 44)'
            })
            enable.addEventListener('mouseout', () => {
                enable.style.backgroundColor = 'rgb(50, 177, 65)'
            })
            enable.addEventListener('mousedown', () => {
                enable.style.backgroundColor = 'rgb(35, 93, 41)'
            })
            enable.addEventListener('mouseup', () => {
                enable.style.backgroundColor = 'rgb(34, 120, 44)'

            })
            mode = 'lightmode'
            lightMode()}

        else if(enable.textContent == 'Enable'){

             // SWITCH COLORS //

            enable.textContent = 'Disable'
            enable.style.backgroundColor = 'rgb(169, 48, 48)'
            
            enable.addEventListener('mouseover', () => {
                enable.style.backgroundColor = 'rgb(133, 40, 40)'
            })
            enable.addEventListener('mouseout', () => {
                enable.style.backgroundColor = 'rgb(169, 48, 48)'
            })
            enable.addEventListener('mousedown', () => {
                enable.style.backgroundColor = 'rgb(99, 40, 40)'
            })
            enable.addEventListener('mouseup', () => {
                enable.style.backgroundColor = 'rgb(133, 40, 40)'

            })
            mode = 'darkmode'
            darkMode()
        }
        }
        contrast_button.onclick = function(){
            if(contrast_button.textContent == 'Disable'){

                // SWITCH COLORS //
    
                contrast_button.textContent = 'Enable'
                contrast_button.style.backgroundColor = 'rgb(50, 177, 65)'
                
                contrast_button.addEventListener('mouseover', () => {
                    contrast_button.style.backgroundColor = 'rgb(34, 120, 44)'
                })
                contrast_button.addEventListener('mouseout', () => {
                    contrast_button.style.backgroundColor = 'rgb(50, 177, 65)'
                })
                contrast_button.addEventListener('mousedown', () => {
                    contrast_button.style.backgroundColor = 'rgb(35, 93, 41)'
                })
                contrast_button.addEventListener('mouseup', () => {
                    contrast_button.style.backgroundColor = 'rgb(34, 120, 44)'
    
                })
                
                boxes.forEach(box => {
                    
                    if(box.style.backgroundColor == correct){
                        box.style.backgroundColor = original_green
                    }
                    else if(box.style.backgroundColor == inside){
                        box.style.backgroundColor = original_yellow
                    }
                })
                correct = original_green
                inside = original_yellow
                

            
            }
                
    
            else if(contrast_button.textContent == 'Enable'){
    
                keys = document.querySelectorAll('.row2 button')
    
                contrast_button.textContent = 'Disable'
                contrast_button.style.backgroundColor = 'rgb(169, 48, 48)'
                
                contrast_button.addEventListener('mouseover', () => {
                    contrast_button.style.backgroundColor = 'rgb(133, 40, 40)'
                })
                contrast_button.addEventListener('mouseout', () => {
                    contrast_button.style.backgroundColor = 'rgb(169, 48, 48)'
                })
                contrast_button.addEventListener('mousedown', () => {
                    contrast_button.style.backgroundColor = 'rgb(99, 40, 40)'
                })
                contrast_button.addEventListener('mouseup', () => {
                    contrast_button.style.backgroundColor = 'rgb(133, 40, 40)'
    
                })
                
                boxes.forEach(box => {
                    
                    if(box.style.backgroundColor == correct){
                        box.style.backgroundColor = contrast_green
                    }
                    else if(box.style.backgroundColor == inside){
                        box.style.backgroundColor = contrast_yellow
                    }
                    
                })

                keys.forEach(key => {
                    if(key.style.backgroundColor == correct){
                        key.style.backgroundColor = contrast_green
                    }
                    else if(key.style.backgroundColor == inside){
                        key.style.backgroundColor = contrast_yellow
                    }
                })
                correct = contrast_green
                inside = contrast_yellow

            }
            
        }
        
    }

    function lightMode(){
        
        CurrentRow()
        headertext.style.color = 'black'
        background.style.backgroundColor = 'rgb(236, 236, 236)'
        boxes.forEach(box => {

            if(box.style.backgroundColor == correct || box.style.backgroundColor == inside || box.style.backgroundColor != none){
                box.style.color = 'white'
            }


    })
        current.forEach(box => {
        
            if(box.style.backgroundColor != correct && box.style.backgroundColor != inside && box.style.backgroundColor != none){
                box.style.color = 'black'
            }
            
            else if(row != 6 && box.style.color != 'white'){
                box.style.color = 'black'
            }
            else if(row == '6'){
                box.style.color = 'white'

            }

        })
        boxes.forEach(box => box.style.borderColor = 'rgb(165, 165, 165)')
        settingsbackground.style.backgroundColor = 'rgb(210, 210, 210)'
        settingsheading.style.color = 'black'
        settingsmode.style.color = 'black'
        contrast_header.style.color = 'black'
        errorMessage.style.backgroundColor = 'black'
        errorMessage.style.color = 'white'
        errorMessage.style.padding = '10px'
        keys = document.querySelectorAll('.row2 button')
        keys.forEach(key => {
            if(key.style.backgroundColor != correct && key.style.backgroundColor != inside){
                key.style.backgroundColor = 'rgb(210, 210, 210)'
                key.style.color = 'black'
            }
            else if(key.style.backgroundColor == correct || key.style.backgroundColor == inside){
                key.style.color = 'white'
            }
        })
        document.getElementById("enterimage").src = 'enterlight.png'
        document.getElementById("backspaceimage").src = 'backspacelight.png'
        console.log(errorMessage)

    }

    function darkMode(){
    
        headertext.style.color = 'white'
        background.style.backgroundColor = 'rgb(26, 25, 25)'
        boxes.forEach(box => box.style.borderColor = 'rgb(68, 66, 66)')
        boxes.forEach(box => box.style.color = 'white')
        settingsbackground.style.backgroundColor = 'rgb(32, 31, 31)'
        settingsheading.style.color = 'white'
        settingsmode.style.color = 'white'
        contrast_header.style.color = 'white'
        errorMessage.style.backgroundColor = 'white'
        errorMessage.style.color = 'black'
        keys = document.querySelectorAll('.row2 button')
        keys.forEach(key => {
            if(key.style.backgroundColor != correct && key.style.backgroundColor != inside){
                key.style.backgroundColor = 'rgb(68, 66, 66)'
            }
            key.style.color = 'white'
        })
        document.getElementById("enterimage").src = 'enterdark.png'
        document.getElementById("backspaceimage").src = 'backspacedark.png'

    }


// WORDS //

const words = wordleWords

const word = words[Math.floor(Math.random() * words.length)]

// UNALLOWED KEYS //

function isOnlyLetters(str) {
    return /^[A-Za-z]+$/.test(str);
}

const disallowedKeys = [
    'Shift', 'Control', 'Alt', 'AltGraph', 'Meta', 'CapsLock', 
    'Escape', 'Pause', 'PrintScreen', 'ScrollLock', 'NumLock', 
    'F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8', 'F9', 'F10', 'F11', 'F12', 
    'ArrowUp', 'ArrowDown', 'ArrowLeft', 'ArrowRight', 'Home', 'End', 'PageUp', 'PageDown', 
    'Delete', 'Insert', 
    'Tab', 'Space', 'Dead'
];

// CURRENT ROW // 

let row = 0
let current;
let index = 0

// VALIDITY //

const errorMessage = document.getElementById('errormessage');

function hideErrorMessage() {
    if (errorMessage.style.opacity != 0){
         errorMessage.style.opacity = 0
    }    
    listening = true
}

let rows_completed = []
let words_guessed = []
const valid_rows = [0,1,2,3,4,5]

function incomplete(){
    
    current[0].style.borderColor = 'rgb(205, 71, 71)'
    current[1].style.borderColor = 'rgb(205, 71, 71)'
    current[2].style.borderColor = 'rgb(205, 71, 71)'
    current[3].style.borderColor = 'rgb(205, 71, 71)'
    current[4].style.borderColor = 'rgb(205, 71, 71)'

    if(mode == 'darkmode'){
        setTimeout(() => {

            current[0].style.borderColor = 'rgb(68, 66, 66)'
            current[1].style.borderColor = 'rgb(68, 66, 66)'
            current[2].style.borderColor = 'rgb(68, 66, 66)'
            current[3].style.borderColor = 'rgb(68, 66, 66)'
            current[4].style.borderColor = 'rgb(68, 66, 66)'
        }, 700)
    }
    else{
        setTimeout(() => {

            current[0].style.borderColor = 'rgb(165, 165, 165)'
            current[1].style.borderColor = 'rgb(165, 165, 165)'
            current[2].style.borderColor = 'rgb(165, 165, 165)'
            current[3].style.borderColor = 'rgb(165, 165, 165)'
            current[4].style.borderColor = 'rgb(165, 165, 165)'
        }, 700)
    }
   

}

function resetColor(){

    if(mode == 'darkmode'){

        current[0].style.borderColor = 'rgb(68, 66, 66)'
        current[1].style.borderColor = 'rgb(68, 66, 66)'
        current[2].style.borderColor = 'rgb(68, 66, 66)'
        current[3].style.borderColor = 'rgb(68, 66, 66)'
        current[4].style.borderColor = 'rgb(68, 66, 66)'

    }
    else{  

        current[0].style.borderColor = 'rgb(165, 165, 165)'
        current[1].style.borderColor = 'rgb(165, 165, 165)'
        current[2].style.borderColor = 'rgb(165, 165, 165)'
        current[3].style.borderColor = 'rgb(165, 165, 165)'
        current[4].style.borderColor = 'rgb(165, 165, 165)'


    }
        
}

function CurrentRow(){
    
    if(row == 0){
        current = First_Row.querySelectorAll('h1')
    }
    else if(row == 1){
        current = Second_Row.querySelectorAll('h1')
    }
    else if(row == 2){
        current = Third_Row.querySelectorAll('h1')
    }
    else if(row == 3){
        current = Fourth_Row.querySelectorAll('h1')
    }
    else if(row == 4){
        current = Fifth_Row.querySelectorAll('h1')
    }
    else if(row == 5){
        current = Sixth_Row.querySelectorAll('h1')
    }
}

CurrentRow()

console.log(word)

function keyboard(id){


    if(!rows_completed.includes(row) && valid_rows.includes(row) && SettingsOpened != 'Y' && listening == true){
    if(mode == 'lightmode'){
        current.forEach(box => {
            if(current != Sixth_Row.querySelectorAll('h1')){
                box.style.color = 'black'
            }
        })
    }
        if(id == 'backspace'){
        
        if(index != 0){   
            index -= 1
            current[index].textContent = ''
        }
    }
    else if(id == 'enter'){
        if(index == 5){

            // GUESS //

            let guess = ''

            for(let i = 0; i < word.length; i++){
                guess += current[i].textContent
            }

            if(wordleWords.includes(guess)){

            resetColor()
            rows_completed.push(row)
            const counts = {};
            let count = 0
            
            // WORDS LETTERS COUNT //

            for (let char of word) {
                counts[char] = (counts[char] || 0) + 1;
            }
            
            // Green Letters //

           for(let i = 0; i < guess.length; i++){
            if(String(word[i]) === String(guess[i])){

                key = document.getElementById(current[i].textContent)
                key.style.backgroundColor = correct
                current[i].style.backgroundColor = correct
                key.style.color = 'white'
                current[i].style.border = 'hidden'

                counts[guess[i]]--
                count += 1
            }}
           // Yellow Letters 
          
           for(let i = 0; i < guess.length; i++){

                if(counts[guess[i]] >= 1 && current[i].style.backgroundColor != correct){
                    key = document.getElementById(current[i].textContent)
                    if(key.style.backgroundColor != correct){
                        key.style.backgroundColor = inside
                        key.style.color = 'white'
                    }
                    current[i].style.backgroundColor = inside
                    current[i].style.border = 'hidden'
                    counts[guess[i]]--
                }}
          
            // Gray Letters
            
           for(let i = 0; i < guess.length; i++){

                    if(current[i].style.backgroundColor != correct && current[i].style.backgroundColor != inside){
                        current[i].style.backgroundColor = none
                        current[i].style.border = 'hidden'
                    }

                }
            
            
                if(mode == 'lightmode'){
                    boxes.forEach(box => {
                        if(box.textContent != ''){
                            box.style.color = 'white'
                        }
                    })
                }
                   
            
            if(count == 5){
                return
            }
            else{
                index = 0
                row += 1
                CurrentRow()

            
            }  
            }
            else{
                
                listening = false
            
                errorMessage.textContent = 'Not in word list'
                
                errorMessage.style.opacity = 1
                
                setTimeout(hideErrorMessage,700)

                incomplete()  
            }
        
           
        }
        
        else{     

            listening = false
            
            errorMessage.textContent = 'Not enough letters'
            
            errorMessage.style.opacity = 1
            
            setTimeout(hideErrorMessage,700)

            incomplete()  
                          
        }
    }  
    else{
        if(index != 5){
            
            current[index].textContent = id
            index++
        }
    }
    }
}


document.addEventListener("keydown", event => {
    if(!rows_completed.includes(row) && valid_rows.includes(row) && SettingsOpened != 'Y' && listening == true){

        if(isOnlyLetters(event.key) && !disallowedKeys.includes(event.key)){

            if(mode == 'lightmode'){
                current.forEach(box => {
                    if(current != Sixth_Row.querySelectorAll('h1')){
                        box.style.color = 'black'
                    }
                })
            }
                if(event.key == 'Backspace'){
                
                if(index != 0){   
                    index -= 1
                    current[index].textContent = ''
                }
            }
            else if(event.key == 'Enter'){
                if(index == 5){

                    // GUESS //

                    let guess = ''

                    for(let i = 0; i < word.length; i++){
                        guess += current[i].textContent
                    }

                    if(wordleWords.includes(guess)){

                    resetColor()
                    rows_completed.push(row)
                    const counts = {};
                    let count = 0
                    
                    // WORDS LETTERS COUNT //

                    for (let char of word) {
                        counts[char] = (counts[char] || 0) + 1;
                    }
                    
                    // Green Letters //
        
                   for(let i = 0; i < guess.length; i++){
                    if(String(word[i]) === String(guess[i])){
                        key = document.getElementById(current[i].textContent)
                        key.style.backgroundColor = correct
                        key.style.color = 'white'
                        
                        current[i].style.backgroundColor = correct
                        current[i].style.border = 'hidden'

                        counts[guess[i]]--
                        count += 1
                    }}
                   // Yellow Letters 
                  
                   for(let i = 0; i < guess.length; i++){

                        if(counts[guess[i]] >= 1 && current[i].style.backgroundColor != correct){
                            key = document.getElementById(current[i].textContent)
                            if(key.style.backgroundColor != correct){
                                key.style.backgroundColor = inside
                                key.style.color = 'white'
                            }
                            
                            current[i].style.backgroundColor = inside
                            current[i].style.border = 'hidden'
                            counts[guess[i]]--
                        }}
                  
                    // Gray Letters
                    
                   for(let i = 0; i < guess.length; i++){

                            if(current[i].style.backgroundColor != correct && current[i].style.backgroundColor != inside){
                                current[i].style.backgroundColor = none
                                current[i].style.border = 'hidden'
                            }

                        }
                    
                    
                        if(mode == 'lightmode'){
                            boxes.forEach(box => {
                                if(box.textContent != ''){
                                    box.style.color = 'white'
                                }
                            })
                        }
                           
                    
                    if(count == 5){
                        return
                    }
                    else{
                        index = 0
                        row += 1
                        CurrentRow()

                    
                    }  
                    }
                    else{
                        
                        listening = false
                    
                        errorMessage.textContent = 'Not in word list'
                        
                        errorMessage.style.opacity = 1
                        
                        setTimeout(hideErrorMessage,700)

                        incomplete()  
                    }
                
                   
                }
                
                else{     

                    listening = false
                    
                    errorMessage.textContent = 'Not enough letters'
                    
                    errorMessage.style.opacity = 1
                    
                    setTimeout(hideErrorMessage,700)

                    incomplete()  
                                  
                }
            }  
            else{
                if(index != 5){
                    
                    current[index].textContent = event.key.toLowerCase()
                    index++
                }
            }
        }
            
    }

})


       

