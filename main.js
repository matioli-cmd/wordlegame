// LOAD WORDS //

let wordleWords;

async function loadWords(){

    try{
        response = await fetch('words.json')
        value = await response.json()
        wordleWords = value.filter(word => word.length == 5)
    }
    catch(error){
        console.log(error)
    }

}

loadWords().then(() => {
    const word = wordleWords[Math.floor(Math.random() * 2124)] 

// END MESSAGE //

let wordmessage = document.getElementById("wordmessage")
let endmessage = document.getElementById("endmessage")
let enddiv = document.getElementById("end")
let playagain = document.getElementById("playagain")

playagain.onclick = function(){
    location.reload()
}

// OTHER VARIABLES // 

let original_green = 'rgb(67, 173, 61)'
let original_yellow = 'rgb(235, 206, 65)'

let contrast_green = 'rgb(191, 57, 33)'
let contrast_yellow = 'rgb(33, 146, 191)'

let correct = 'rgb(67, 173, 61)'
let inside = 'rgb(235, 206, 65)'
let none = 'rgb(67, 67, 66)'
let keyboardnone = 'rgb(36, 36, 36)'

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
    errorMessage.style.opacity = 0
    
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

                keys = document.querySelectorAll('.row2 button')

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
                keys.forEach(key => {
                    if(key.style.backgroundColor == correct){
                        key.style.backgroundColor = original_green
                    }
                    else if(key.style.backgroundColor == inside){
                        key.style.backgroundColor = original_yellow
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
            if(key.style.backgroundColor == correct || key.style.backgroundColor == inside || key.style.backgroundColor == keyboardnone){
                if(key.style.backgroundColor == keyboardnone){
                    key.style.backgroundColor = none
                }
                key.style.color = 'white'
            }
            else if(key.style.backgroundColor != correct && key.style.backgroundColor != inside && key.style.backgroundColor != keyboardnone){
                key.style.backgroundColor = 'rgb(210, 210, 210)'
                key.style.color = 'black'
            }

        })
        document.getElementById("enterimage").src = 'enterlight.png'
        document.getElementById("backspaceimage").src = 'backspacelight.png'
        wordmessage.style.color = 'white'
        wordmessage.style.backgroundColor = 'black'
        endmessage.style.color = 'black'
        enddiv.style.backgroundColor = 'rgb(185, 185, 185)'

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
            if(key.style.backgroundColor == none){
                key.style.backgroundColor = keyboardnone
            }
            if(key.style.backgroundColor != correct && key.style.backgroundColor != inside && key.style.backgroundColor != keyboardnone){
                key.style.backgroundColor = 'rgb(68, 66, 66)'
            }
            key.style.color = 'white'
        })
        document.getElementById("enterimage").src = 'enterdark.png'
        document.getElementById("backspaceimage").src = 'backspacedark.png'
        wordmessage.style.color = 'black'
        wordmessage.style.backgroundColor = 'white'
        endmessage.style.color = 'white'
        enddiv.style.backgroundColor = 'rgb(32, 31, 31)'

    }

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

                    key = document.getElementById(current[i].textContent)
                    if(current[i].style.backgroundColor != correct && current[i].style.backgroundColor != inside){
                        if(key.style.backgroundColor != correct && key.style.backgroundColor != inside){
                            if(mode == 'lightmode'){
                                key.style.backgroundColor = none
                                key.style.color = 'white'
                            }
                            else if(mode == 'darkmode'){
                                key.style.backgroundColor = keyboardnone
                                key.style.color = 'white'
                            }

                        }
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
                setTimeout(() => {
                    enddiv.style.visibility = 'visible'
                    enddiv.style.opacity = '1'
                    endmessage.textContent = 'GOOD JOB'
                    wordmessage.textContent = word}, 1000)
                return
            }
            else{
                index = 0
                row += 1
                CurrentRow()
                if(row == 6){
                    setTimeout(() => {
                        enddiv.style.visibility = 'visible'
                        enddiv.style.opacity = '1'
                        endmessage.textContent = 'GOOD TRY'
                        wordmessage.textContent = word}, 1000)
                   return
                }

            
            }  
            }
            else{
                
                listening = false
            
                errorMessage.textContent = 'Not in word list'
                
                errorMessage.style.opacity = 1
                
                setTimeout(hideErrorMessage,700)
            }
        
           
        }
        
        else{     

            listening = false
            
            errorMessage.textContent = 'Not enough letters'
            
            errorMessage.style.opacity = 1
            
            setTimeout(hideErrorMessage,700) 
                          
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
                                key = document.getElementById(current[i].textContent)
                                if(key.style.backgroundColor != correct && key.style.backgroundColor != inside){
                                    if(mode == 'lightmode'){
                                        key.style.backgroundColor = none
                                        key.style.color = 'white'
                                    }
                                    else if(mode == 'darkmode'){
                                        key.style.backgroundColor = keyboardnone
                                        key.style.color = 'white'
                                    }

                                }
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
                        setTimeout(() => {
                            enddiv.style.visibility = 'visible'
                            enddiv.style.opacity = '1'
                            endmessage.textContent = 'GOOD JOB'
                            wordmessage.textContent = word}, 1000)
                            return
                    }
                    else{
                        index = 0
                        row += 1
                        CurrentRow()
                        if(row == 6){
                            setTimeout(() => {
                                enddiv.style.visibility = 'visible'
                                enddiv.style.opacity = '1'
                                endmessage.textContent = 'GOOD TRY'
                                wordmessage.textContent = word}, 1000)
                                return
                            
                           
                        }

                    
                    }  
                    }
                    
                    else{
                        
                        listening = false
                    
                        errorMessage.textContent = 'Not in word list'
                        
                        errorMessage.style.opacity = 1
                        
                        setTimeout(hideErrorMessage,700)
                    }
                
                   
                }
                
                else{     

                    listening = false
                    
                    errorMessage.textContent = 'Not enough letters'
                    
                    errorMessage.style.opacity = 1
                    
                    setTimeout(hideErrorMessage,700)
                                  
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

})
