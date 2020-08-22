import 'regenerator-runtime/runtime'
import preval from 'preval.macro'

import * as rts from '../src/rts.mjs'
import req from '../src/js-exports.req.mjs'

import CodeMirror from 'codemirror'
import 'codemirror/mode/javascript/javascript'
import 'codemirror/lib/codemirror.css'
import '@alenaksu/json-viewer'

let init = async () => {
  let wasmModule = await WebAssembly.compileStreaming(fetch('js-exports.wasm'))
  let {
    exports: { hs_init, parseSrc },
  } = await rts.newAsteriusInstance({ ...req, module: wasmModule })

  hs_init()

  let editor = CodeMirror.fromTextArea(document.getElementById('src'), {
    mode: {
      name: 'javascript',
    },
    tabSize: 2,
    lineNumbers: true,
  })

  editor.on('changes', async (doc) => {
    document.getElementById('ast').data = JSON.parse(
      await parseSrc(doc.getValue())
    )
  })

  editor.setValue(
    preval`module.exports = require('fs').readFileSync('../test/example.js').toString()`
  )
}

init()
