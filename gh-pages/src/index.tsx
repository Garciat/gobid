export default (_data: Lume.Data, h: Lume.Helpers) => {
  return (
    <html lang="en">
      <head>
        <meta charset="UTF-8" />
        <title>gobid</title>

        <link
          rel="stylesheet"
          href="https://cdn.jsdelivr.net/npm/vscode-codicons@0.0.17/dist/codicon.min.css"
        />

        <link rel="stylesheet" href={h.url("/main.css")} />

        <script type="module" src={h.url("/main.js")} />
      </head>
      <body>
        <div class="container">
          <div class="pane">
            <div class="tabs">
              <template id="tab-template">
                <div class="tab" draggable={true}>
                  <input type="radio" id="tab-n" name="tab" value="tab1" />
                  <label for="tab-n">
                    <span class="tab-name"></span>
                    <div class="close-tab codicon codicon-chrome-close"></div>
                  </label>
                </div>
              </template>
              <div class="newtab" id="newtab">
                <div class="codicon codicon-add"></div>
              </div>
            </div>
            <div class="editor" id="input"></div>
          </div>
          <div class="pane">
            <div class="editor" id="output"></div>
          </div>
        </div>
      </body>
    </html>
  );
};
