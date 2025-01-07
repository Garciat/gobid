import monaco from "./monaco.ts";
import { gobid } from "./lib/gobid.ts";

monaco.languages.register({ id: "gobid-output" });

monaco.languages.setMonarchTokensProvider("gobid-output", {
  goTypeKeywords: [
    "type",
    "interface",
    "struct",
    "func",
  ],
  goTypes: [
    "string",
    "bool",
    "int",
    "int8",
    "int16",
    "int32",
    "int64",
    "uint",
    "uint8",
    "uint16",
    "uint32",
    "uint64",
    "float32",
    "float64",
    "complex64",
    "complex128",
    "byte",
    "rune",
    "uintptr",
    "any",
  ],
  gobidKeywords: [
    "sat",
  ],
  operators: [
    "=",
    "<:",
    "?",
    "->",
  ],
  symbols: /[=><!~?:&|+\-*\/\^%]+/,
  tokenizer: {
    root: [
      [/===/, "number", "@section"],
      [/DEFINING( TYPE)?/, "comment"],
      [/ERROR/, "invalid"],
      [/@[A-Za-z-_0-9]+/, "annotation"],
      [/[a-z_$][\w$]*/, {
        cases: {
          "@goTypeKeywords": "keyword",
          "@goTypes": "type",
          "@gobidKeywords": "regexp",
          "@default": "identifier",
        },
      }],
      [/[{}()\[\]]/, "@brackets"],
      [/[<>](?!@symbols)/, "@brackets"],
      [/@symbols/, {
        cases: {
          "@operators": "regexp",
          "@default": "",
        },
      }],
    ],
    section: [
      [/===/, "number", "@pop"],
      [/[(]/, "string", "@sectionTitle"],
      [/./, "string"],
    ],
    sectionTitle: [
      [/[)]/, "string", "@pop"],
      [/./, "namespace"],
    ],
  },
});

async function compile(name: string, source: string): Promise<string> {
  return await gobid({ "/main.go": source });
}

const DefaultContent = await fetch(import.meta.resolve("./default.go")).then((
  res,
) => res.text());

function getDefaultDocument(): SourceDocument {
  return {
    id: crypto.randomUUID(),
    name: "main.go",
    content: DefaultContent,
    model: monaco.editor.createModel(DefaultContent, "go"),
  };
}

const StorageKeyDocumentPrefix = "gobid-document-";

type SourceDocumentID = string;

interface SourceDocument {
  id: SourceDocumentID;
  name: string;
  content: string;
  model: monaco.editor.ITextModel;
}

function loadDocuments(): Array<SourceDocument> {
  const documents: Array<SourceDocument> = [];

  for (let i = 0;; ++i) {
    const key = localStorage.key(i);
    if (key === null) {
      break;
    }

    if (!key.startsWith(StorageKeyDocumentPrefix)) {
      continue;
    }

    const data = localStorage.getItem(key) ?? "{}";

    try {
      const doc = JSON.parse(data) as SourceDocument;
      doc.model = monaco.editor.createModel(doc.content, "go");
      documents.push(doc);
    } catch (e) {
      console.error(e);
    }
  }

  return documents;
}

function saveDocument(doc: SourceDocument) {
  const key = `${StorageKeyDocumentPrefix}${doc.id}`;
  const data = JSON.stringify({
    id: doc.id,
    name: doc.name,
    content: doc.model.getValue(),
  });
  localStorage.setItem(key, data);
  console.log("saved document:", doc.id, doc.name);
}

function deleteDocument(id: SourceDocumentID) {
  const key = `${StorageKeyDocumentPrefix}${id}`;
  localStorage.removeItem(key);
  console.log("deleted document:", id);
}

const StorageKeySelectedTab = "gobid-selected-tab";

function saveSelectedTab(id: SourceDocumentID) {
  localStorage.setItem(StorageKeySelectedTab, id);
}

function loadSelectedTab() {
  return localStorage.getItem(StorageKeySelectedTab);
}

const StorageKeyTabOrder = "gobid-tab-order";

function saveTabOrder(ids: Array<SourceDocumentID>) {
  localStorage.setItem(StorageKeyTabOrder, JSON.stringify(ids));
}

function loadTabOrder(): Array<SourceDocumentID> {
  const data = localStorage.getItem(StorageKeyTabOrder);
  return data === null ? [] : JSON.parse(data);
}

function debounce<Args extends unknown[]>(
  wait: number,
  callback: (...args: Args) => void,
): (...args: Args) => void {
  let timeoutId: number | null = null;
  return (...args) => {
    globalThis.clearTimeout(timeoutId ?? 0);
    timeoutId = globalThis.setTimeout(() => {
      callback(...args);
    }, wait);
  };
}

function parseFilename(input: string | null): string | null {
  if (input === null || input === undefined) {
    return null;
  }
  if (input === "") {
    return null;
  }
  if (input.indexOf("/") !== -1) {
    return null;
  }
  if (!input.endsWith(".go")) {
    input += ".go";
  }
  return input;
}

function createTab(id: SourceDocumentID, name: string) {
  const template = document.querySelector(
    "#tab-template",
  ) as HTMLTemplateElement;
  const container = template.content.cloneNode(true) as Element;
  const tab = container.querySelector(".tab") as HTMLDivElement;
  const input = tab.querySelector("input")!;
  const label = tab.querySelector("label")!;
  const tabName = label.querySelector(".tab-name") as HTMLSpanElement;
  const closeTab = label.querySelector(".close-tab")!;

  const tabId = `tab-${id}`;

  tab.dataset.id = id;
  input.id = tabId;
  input.value = name;
  label.setAttribute("for", tabId);
  tabName.textContent = name;

  return { tab, label, tabName, input, closeTab };
}

class Tabs {
  private readonly onTabAddRequest: () => Promise<
    { id: string; name: string } | null
  >;
  private readonly onTabAdded: (id: string) => Promise<void>;
  private readonly onTabSelected: (id: string) => Promise<void>;
  private readonly onTabClosed: (id: string) => void;
  private readonly onTabsReordered: (ids: string[]) => void;
  private readonly onTabRenameRequest: (
    id: string,
    name: string,
  ) => string | null;
  private readonly onTabRenamed: (id: string, name: string) => void;

  private readonly tabs: HTMLElement;
  private readonly newtab: HTMLElement;

  private dragSelectedTab: HTMLElement | null;

  constructor(
    {
      onTabAddRequest,
      onTabAdded,
      onTabSelected,
      onTabClosed,
      onTabsReordered,
      onTabRenameRequest,
      onTabRenamed,
    }: {
      onTabAddRequest: () => Promise<{ id: string; name: string } | null>;
      onTabAdded: (id: string) => Promise<void>;
      onTabSelected: (id: string) => Promise<void>;
      onTabClosed: (id: string) => void;
      onTabsReordered: (ids: string[]) => void;
      onTabRenameRequest: (id: string, name: string) => string | null;
      onTabRenamed: (id: string, name: string) => void;
    },
  ) {
    this.onTabAddRequest = onTabAddRequest;
    this.onTabAdded = onTabAdded;
    this.onTabSelected = onTabSelected;
    this.onTabClosed = onTabClosed;
    this.onTabsReordered = onTabsReordered;
    this.onTabRenameRequest = onTabRenameRequest;
    this.onTabRenamed = onTabRenamed;

    this.tabs = document.querySelector(".tabs")!;
    this.newtab = document.querySelector("#newtab")!;

    this.dragSelectedTab = null;

    this.newtab.addEventListener("click", () => {
      this.requestTabAdd();
    });
  }

  createTab(id: SourceDocumentID, name: string, autoselect = false) {
    const { tab, label, tabName, input, closeTab } = createTab(id, name);

    tab.addEventListener("dragstart", (ev) => {
      // ev.dataTransfer.allowEffect = 'move';
      ev.dataTransfer?.setData("text/plain", "");
      tab.classList.add("dragging");
      this.dragSelectedTab = tab;
    });

    tab.addEventListener("dragover", (ev) => {
      ev.preventDefault();
      if (tab === this.dragSelectedTab) {
        return;
      }
      const i = this.indexOfTab(this.dragSelectedTab!);
      const j = this.indexOfTab(tab);
      {
        const mx = ev.clientX - tab.getBoundingClientRect().left;
        const cx = this.dragSelectedTab!.clientWidth;
        if (mx >= cx / 2) {
          return;
        }
      }
      if (i < j) {
        this.tabs.insertBefore(this.dragSelectedTab!, tab.nextSibling);
      } else {
        this.tabs.insertBefore(this.dragSelectedTab!, tab);
      }
      this.onTabsReordered(this.getTabOrder());
    });

    tab.addEventListener("dragend", () => {
      tab.classList.remove("dragging");
      this.dragSelectedTab = null;
    });

    label.addEventListener("contextmenu", (ev) => {
      ev.preventDefault();

      tabName.dataset.original = tabName.textContent!.trim();
      tabName.contentEditable = "true";
      tabName.focus();

      const selection = globalThis.getSelection();
      if (selection === null) {
        return;
      }

      const range = document.createRange();
      range.selectNodeContents(tabName);
      selection.removeAllRanges();
      selection.addRange(range);
    });

    tabName.addEventListener("keydown", (ev) => {
      if (ev.key === "Enter") {
        ev.preventDefault();
        tabName.blur();
      } else if (ev.key === "Escape") {
        ev.preventDefault();
        tabName.textContent = tabName.dataset.original!;
        tabName.blur();
      }
    });

    tabName.addEventListener("blur", () => {
      tabName.contentEditable = "false";

      const name = this.onTabRenameRequest(id, tabName.textContent!.trim());
      if (name === null) {
        tabName.textContent = tabName.dataset.original!;
      } else {
        tabName.textContent = name;
        this.onTabRenamed(tab.dataset.id!, name);
      }
    });

    input.addEventListener("change", () => {
      this.onTabSelected(id);
    });

    closeTab.addEventListener("click", (ev) => {
      ev.stopPropagation();
      ev.preventDefault();
      this.requestTabClose(tab);
    });

    this.tabs.insertBefore(tab, this.newtab);

    if (autoselect) {
      input.click();
    }
  }

  loadTabsFromDocuments(documents: SourceDocument[]) {
    for (let i = 0; i < documents.length; i++) {
      const doc = documents[i];
      this.createTab(doc.id, doc.name);
    }
  }

  selectTabId(id: SourceDocumentID) {
    const tabs = this.allTabs();
    for (const tab of tabs) {
      if (tab.dataset.id === id) {
        this.selectTab(tab);
        return;
      }
    }
  }

  getSelectedTabId() {
    const tab = this.getSelectedTab();
    return tab === null ? null : tab.dataset.id!;
  }

  getSelectedTab() {
    return this.tabs.querySelector(".tab:has(input:checked)") as HTMLDivElement;
  }

  getTabOrder() {
    return this.allTabs().map((tab) => tab.dataset.id ?? "");
  }

  applyTabOrder(ids: SourceDocumentID[]) {
    const tabs = this.allTabs();
    const tabMap = new Map(
      Array.from(tabs).map((tab) => [tab.dataset.id, tab]),
    );
    for (const id of ids) {
      const tab = tabMap.get(id);
      if (tab !== undefined) {
        this.tabs.insertBefore(tab, this.newtab);
      }
    }
  }

  async requestTabAdd() {
    const header = await this.onTabAddRequest();
    if (header === null) {
      return;
    }
    const { id, name } = header;
    this.createTab(id, name, true);
    await this.onTabAdded(id);
  }

  requestTabClose(tab: HTMLElement) {
    if (!globalThis.confirm("Are you sure you want to close this tab?")) {
      return;
    }

    const index = this.indexOfTab(tab);

    tab.parentElement!.removeChild(tab);
    this.onTabClosed(tab.dataset.id!);

    if (this.isTabSelected(tab)) {
      const next = this.getTabByIndexBounded(index);
      if (next !== undefined) {
        this.selectTab(next);
      }
    }
  }

  isTabSelected(tab: HTMLElement) {
    return tab.querySelector("input:checked") !== null;
  }

  indexOfTab(tab: HTMLElement) {
    const tabs = Array.from(this.tabs.querySelectorAll(".tab"));
    return tabs.indexOf(tab);
  }

  getTabByIndexBounded(index: number) {
    const tabs = Array.from(this.tabs.querySelectorAll<HTMLElement>(".tab"));
    return tabs[Math.max(0, Math.min(index, tabs.length - 1))];
  }

  selectTab(tab: HTMLElement) {
    const input = tab.querySelector("input")!;
    input.click();
  }

  allTabs() {
    return Array.from(this.tabs.querySelectorAll(".tab")) as HTMLDivElement[];
  }
}

async function main() {
  const documents = loadDocuments();
  {
    if (documents.length === 0) {
      documents.push(getDefaultDocument());
      saveDocument(documents[0]);
    }
    for (const doc of documents) {
      console.log("loaded document:", doc.id, doc.name);
    }
  }

  monaco.editor.setTheme("vs-dark");

  const emptyModel = monaco.editor.createModel("");

  const editorInput = monaco.editor.create(document.querySelector("#input")!, {
    automaticLayout: true,
  });

  const editorOutput = monaco.editor.create(
    document.querySelector("#output")!,
    {
      language: "gobid-output",
      automaticLayout: true,
      readOnly: true,
    },
  );

  async function onTabAddRequest() {
    let name = globalThis.prompt("Enter file name");
    name = parseFilename(name);
    if (name === null) {
      return null;
    }

    const id = crypto.randomUUID();

    documents.push({
      id: id,
      name: name,
      content: "",
      model: monaco.editor.createModel("package main\n", "go"),
    });

    return { id, name };
  }

  async function onTabAdded(id: SourceDocumentID) {
    await save(id);
  }

  async function onTabSelected(id: SourceDocumentID) {
    const doc = documents.find((doc) => doc.id === id);
    if (doc === undefined) {
      editorInput.setModel(emptyModel);
      return;
    }
    editorInput.setModel(doc.model);
    saveSelectedTab(id);
    await refresh();
  }

  function onTabClosed(id: SourceDocumentID) {
    const index = documents.findIndex((doc) => doc.id === id);
    documents.splice(index, 1);
    deleteDocument(id);

    if (documents.length === 0) {
      editorInput.setModel(null);
      editorOutput.getModel()!.setValue("");
    }
  }

  function onTabRenameRequest(_id: SourceDocumentID, name: string) {
    return parseFilename(name);
  }

  async function onTabRenamed(id: SourceDocumentID, name: string) {
    const doc = documents.find((doc) => doc.id === id);
    if (doc === undefined) {
      return;
    }
    doc.name = name;
    saveDocument(doc);
    await refresh();
  }

  function onTabsReordered(ids: SourceDocumentID[]) {
    saveTabOrder(ids);
  }

  const tabs = new Tabs({
    onTabAddRequest,
    onTabAdded,
    onTabSelected,
    onTabClosed,
    onTabsReordered,
    onTabRenameRequest,
    onTabRenamed,
  });
  tabs.loadTabsFromDocuments(documents);
  tabs.applyTabOrder(loadTabOrder());
  {
    let id = loadSelectedTab();
    if (id === null) {
      id = documents[0].id;
    }
    const doc = documents.find((doc) => doc.id === id);
    if (doc === undefined) {
      id = documents[0].id;
    }
    tabs.selectTabId(id);
  }

  async function refresh() {
    const id = tabs.getSelectedTabId();
    const doc = documents.find((doc) => doc.id === id);
    if (doc === undefined) {
      return;
    }
    const output = await compile(doc.name, doc.model.getValue());
    editorOutput.getModel()!.setValue(output);
  }

  async function save(id: SourceDocumentID | null) {
    const doc = documents.find((doc) => doc.id === id);
    if (!doc) {
      return;
    }
    saveDocument(doc);
    saveTabOrder(tabs.getTabOrder());
  }

  const debouncedRefresh = debounce(200, refresh);
  editorInput.onDidChangeModelContent(() => save(tabs.getSelectedTabId()));
  editorInput.onDidChangeModelContent(() => debouncedRefresh());
}

await main();
