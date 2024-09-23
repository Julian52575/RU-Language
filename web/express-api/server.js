const express = require('express');
const cors = require('cors')
const bodyParser = require('body-parser');
const { spawn } = require('child_process');
const path = require('path');
const app = express();
const port = 3000;

app.use(cors());
app.use(bodyParser.json());

app.get('/', (req, res) => {
  res.send('Hello from Express API!');
});

app.post('/lisp', (req, res) => {
  console.log(req.body);

  const command = path.join(__dirname, 'my-lisp-interpreter-exe'); // interpretor should be in express-api/
  try {

    const process = spawn(command);
    
    process.stdin.write(req.body.code);
    process.stdin.end();
    
    let output = '';
    let errorOutput = '';
    
    process.stdout.on('data', (data) => {
      output += data.toString();
    });
    
    process.stderr.on('data', (data) => {
      errorOutput += data.toString();
    });
    
    process.on('close', (code) => {
      if (code !== 0) {
        console.error(`Process exited with code ${code}`);
        return res.status(500).json({ error: errorOutput || 'Internal Server Error' });
      }
      res.json({ output });
    });
    
    process.on('error', (err) => {
      console.error(`Failed to start process: ${err}`);
      res.status(500).json({ error: 'Failed to start process' });
    });
  }
  catch (err) {
    console.error(err);
    res.status(500).json({ error: 'Internal Server Error' });
  }
});
  
  app.listen(port, () => {
  console.log(`Express API running on http://localhost:${port}`);
});
