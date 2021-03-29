import { resolve } from 'path'
import { minifyHtml, injectHtml } from 'vite-plugin-html'

const scalaVersion = '2.13'
// const scalaVersion = '3.0.0-RC1'

// https://vitejs.dev/config/
export default ({ mode }) => {
  const mainJS = `/target/scala-${scalaVersion}/formula-${mode === 'production' ? 'opt' : 'fastopt'}/main.js`
  console.log('mainJS', mainJS)
  return {
    publicDir: './src/main/static/public',
    plugins: [
      ...(process.env.NODE_ENV === 'production' ? [
        minifyHtml(),
      ] : []),
      injectHtml({
        injectData: {
          mainJS
        }
      })
    ],
    resolve: {
      alias: {
        'stylesheets': resolve(__dirname, './src/main/static/stylesheets'),
      }
    }
  }
}
