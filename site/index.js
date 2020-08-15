import 'regenerator-runtime/runtime'

import * as rts from '../src/rts.mjs'
import req from '../src/js-exports.req.mjs'

import CodeMirror from 'codemirror'
import 'codemirror/mode/javascript/javascript'
import 'codemirror/lib/codemirror.css'

let init = async () => {
  let wasmModule = await WebAssembly.compileStreaming(fetch('js-exports.wasm'))
  let {
    exports: { hs_init, parseSrc },
  } = await rts.newAsteriusInstance(Object.assign(req, { module: wasmModule }))

  hs_init()

  let $ast = document.getElementById('ast')
  let editor = CodeMirror.fromTextArea(document.getElementById('src'), {
    mode: {
      name: 'javascript',
    },
    tabSize: 2,
    lineNumbers: true,
  })

  editor.on('change', async (doc) => {
    $ast.textContent = await parseSrc(doc.getValue())
  })

  editor.setValue(`function cha(a, b) {
  var some = 42
}

const arr = [123]

let obj = {
  a: 1,
  b: cha
}

cha('hello', ((("world"))), true)`)
}

init()
