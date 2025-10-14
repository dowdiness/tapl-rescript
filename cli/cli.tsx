#!/usr/bin/env node
import React from 'react';
import Pastel from 'pastel';

const app = new Pastel({
	importMeta: import.meta,
});

await app.run();
