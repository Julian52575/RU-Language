// express-api/server.js

const express = require('express');
const app = express();
const port = 3000;

app.get('/', (req, res) => {
  res.send('Hello from Express API!');
});

app.listen(port, () => {
  console.log(`Express API running on http://localhost:${port}`);
});
