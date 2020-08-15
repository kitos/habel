import 'regenerator-runtime/runtime'

import * as rts from '../src/rts.mjs'
import req from '../src/js-exports.req.mjs'

let init = async () => {
  let wasmModule = await WebAssembly.compileStreaming(fetch('js-exports.wasm'))
  let {
    exports: { hs_init, parseSrc },
  } = await rts.newAsteriusInstance(Object.assign(req, { module: wasmModule }))

  hs_init()

  let $src = document.getElementById('src')
  let $ast = document.getElementById('ast')

  $src.addEventListener('keyup', async () => {
    $ast.textContent = await parseSrc($src.value ?? '')
  })
}

init()
