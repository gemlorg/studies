var root = document.querySelector(":root");
var fid = 1;
var upload_id = 1;
var filename = "";
var text = "";


const LogIn = () => {
  window.location.href= '/polls/accounts/login'
}

const LogOut = () => {
  window.location.href = '/polls/accounts/logout'
}

const OpenFile = async (id, name) => {
  const response = await fetch("get_file_text?id=" + id);
  const data = await response.json();

  var html_string = "";
  var i = 1
  for (const line of data["code"]) {
    html_string += "<div class='code_line' id='line_" + i +"'><code>" + line + "\n" +"</code></div>";
    i++
  }
  fid = id;
  filename = name;

  document.getElementById("code").innerHTML = html_string;
  SaveFile();
};
const DownloadFile = async () => {
  text = document.getElementById("fragment").textContent;
  download(text, "compiled.txt", "text/plain");
};

//compile
const SaveFile = async () => {
  const response = await fetch("get_compile_file?id=" + fid);
  const data = await response.json();

  var html_string = "";
  var i = 1;
  var is_warning = new RegExp("\.\\/c\\/.*$")
  document.getElementById("fragment").innerHTML = ""

  for (const line of data["code"]) {
    if(is_warning.test(line)) {
      var sub = line.split(":")[1]
      console.log(line + " is a warning, sub is "  + sub)
      document.getElementById("fragment").innerHTML += "<div class='fragment_code_line' onmouseover='HighlightLines("+i+","+sub+")' onmouseout='NormalLines("+i+","+sub+")' id='fragment_line_" +i+"'>"+line + "\n"+"<div>";
      // try{
        
      //   HighlightLines(i, sub)
      
      // } catch (exception){}
      
    }else {
      document.getElementById("fragment").innerHTML += "<div class='fragment_code_line' id='fragment_line_" +i+"'><code>"+line+"</code><div>";
    }
    i++
  }
  //  = html_string;
 
};
async function  HighlightLines  (f, c) {
  document.getElementById('fragment_line_'+f).style.backgroundColor = "yellow"
  document.getElementById('line_'+c).style.backgroundColor = "yellow"
  // document.getElementById('line_'+c).scrollIntoView()
  location.href = "#line_" + c
}
async function NormalLines(f, c) {
  document.getElementById('fragment_line_'+f).style.backgroundColor = getComputedStyle(root).getPropertyValue("--code-background")
  document.getElementById('line_'+c).style.backgroundColor  = getComputedStyle(root).getPropertyValue("--code-background")
}



function ShowDependencies() {
  var option = document.getElementById("processor-select").value;

  var strr = "";
  if (option == "mcs51") {
    strr = ` <p>Model: </p>
          <select name="dependencies">
            <option value="--model-small">Small</option>
            <option value="--model-medium">Medium</option>
            <option value="--model-large">Large</option>
          </select><br>
          <input type="checkbox" name="dependencies" id="dependencies2" value="--xstack">
          <label for="dependencies2">Uses a pseudo stack in the __pdata area</label><br>
          <input type="checkbox" name="dependencies" id="dependencies3" value="--acall-ajmp">
          <label for="dependencies3">Replaces the three byte instructions lcall/ljmp with the two byte instructions acall/ajmp</label><br>
          <input type="submit" class="button"> `;
  }
  if (option == "z80") {
    strr = ` <p>Define assembler name: </p>
          <select name="dependencies">
            <option value="--asm=rgbds">rgbds</option>
            <option value="--asm=sdasz80">sdasz80</option>
            <option value="--asm=z80asm">z80asm</option>
            <option value="--asm=isas">isas</option>
          </select><br>
          <input type="checkbox" name="dependencies" id="dependencies2" value="--fno-omit-frame-pointer">
          <label for="dependencies2">Never omit the frame pointer.</label><br>
          <input type="checkbox" name="dependencies" id="dependencies3" value="--portmode=z80">
          <label for="dependencies3">Determinate PORT I/O mod</label><br>
          <input type="submit" class="button">  `;
  }
  if (option == "stm8") {
    strr = `
    <p>Model: </p>
           <select name="dependencies">
             <option value="--model-medium">Medium</option>
             <option value="--model-large">Large</option>
           </select><br>
           <input type="submit" class="button"></input>`;
  }
  document.getElementById("tabs__content").innerHTML = strr;
}

const DeleteFolder = async (id) => {
  post("delete_folder",{id: id});
};

const DeleteFile = async (id) => {
  post("delete_file", { id: id });
};


const AddFile = async (id) => {
  upload_id = id;

  document
    .querySelector('input[type="file"]')
    .addEventListener("change", (e) => {
      let reader = new FileReader();
      var file = e.target.files[0];
      reader.readAsText(file);
      reader.onload = function () {
        post("file_receiver", {
          folder_id: id,
          name: file.name,
          code: reader.result,
        });
      };
      // path to server would be where you'd normally post the form to
    });
  const i = document.querySelector('input[type="file"]').click();
};

const  post =  async (path, params, method = "post") => {
  params['csrfmiddlewaretoken'] =  csrftoken 

  const form = document.createElement("form");
  form.method = method;
  form.action = path;
// const fi = document.createElement("input");
//   fi.type = "hidden"
//   fi.name = 'csrfmiddlewaretoken'

  // fi.vaule = csrftoken 
  for (const key in params) {
    if (params.hasOwnProperty(key)) {
      const hiddenField = document.createElement("input");
      hiddenField.type = "hidden";
      hiddenField.name = key;
      hiddenField.value = params[key];

      form.appendChild(hiddenField);
    }
  }
  
  // form.appendChild(fi)

  document.body.appendChild(form);
  form.submit();
}

function ChangeBackground() {
  var rs = getComputedStyle(root);
  if (rs.getPropertyValue("--background-color") != "#fffffff6") {
    root.style.setProperty("--background-color", "#fffffff6");
    root.style.setProperty("--code-background", "#fffffff6");
    root.style.setProperty("--menu-background", "#fffffff6");
    root.style.setProperty("--fragment-color", "#fffffff6");
    root.style.setProperty("--top-background", "#fffffff6");
    root.style.setProperty("--button-color", "#fff");
    root.style.setProperty("--button-text-color", "#3D3D3D");
    root.style.setProperty("--border-color", "black");
    //root.style.setProperty('--tab-text-color', '#009578');
  } else {
    root.style.setProperty("--background-color", "black");
    root.style.setProperty("--code-background", "black");
    root.style.setProperty("--menu-background", "black");
    root.style.setProperty("--fragment-color", "black");
    root.style.setProperty("--top-background", "black");
    root.style.setProperty("--button-color", "black");
    root.style.setProperty("--button-text-color", "white");
    root.style.setProperty("--border-color", "white");
    //root.style.setProperty('--tab-text-color', '#009578');
  }
}

function showMenu() {
  root.style.setProperty("--width-chooser", "257px");
  root.style.setProperty("--burger-display", "block");
  document.getElementById("burger2").style.display = "none";
}
function hideMenu() {
  root.style.setProperty("--width-chooser", "0px");
  root.style.setProperty("--burger-display", "none");
  document.getElementById("burger2").style.display = "block";
}

function ShowOptions(id, options_id) {
  var visible = "";
  if (document.getElementById(options_id).style.display == "none") {
    visible = "block";
  } else {
    visible = "none";
  }
  document.getElementById(options_id).style.display = visible;
}
