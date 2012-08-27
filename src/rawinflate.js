/**
 * JavaScript Inflate Library
 *
 * The MIT License
 *
 * Copyright (c) 2012 imaya
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

goog.provide('ZlibStat.RawInflate');

goog.require('ZlibStat.Huffman');

//-----------------------------------------------------------------------------

/** @define {boolean} export symbols. */
var ZLIBSTAT_RAW_INFLATE_EXPORT = false;

/** @define {number} buffer block size. */
var ZLIB_BUFFER_BLOCK_SIZE = 0x8000; // [ 0x8000 >= ZLIB_BUFFER_BLOCK_SIZE ]

//-----------------------------------------------------------------------------

goog.scope(function() {

var buildHuffmanTable = ZlibStat.Huffman.buildHuffmanTable;

/**
 * @constructor
 * @param {!(Uint8Array|Array.<number>)} input input buffer.
 * @param {Object} opt_params option parameter.
 *
 * opt_params は以下のプロパティを指定する事ができます。
 *   - index: input buffer の deflate コンテナの開始位置.
 *   - blockSize: バッファのブロックサイズ.
 *   - bufferType: ZlibStat.RawInflate.BufferType の値によってバッファの管理方法を指定する.
 */
ZlibStat.RawInflate = function(input, opt_params) {
  /** @type {!(Array.<number>|Uint8Array)} inflated buffer */
  this.buffer;
  /** @type {!Array.<Object>} */
  this.block = [];
  /** @type {!Array.<(Array.<number>|Uint8Array)>} */
  this.compressed = [];
  /** @type {number} block size. */
  this.blockSize = ZLIB_BUFFER_BLOCK_SIZE;
  /** @type {!number} total output buffer pointer. */
  this.totalpos = 0;
  /** @type {!number} input buffer pointer. */
  this.ip = 0;
  /** @type {!number} bit stream reader buffer. */
  this.bitsbuf = 0;
  /** @type {!number} bit stream reader buffer size. */
  this.bitsbuflen = 0;
  /** @type {!(Array.<number>|Uint8Array)} input buffer. */
  this.input = USE_TYPEDARRAY ? new Uint8Array(input) : input;
  /** @type {!(Uint8Array|Array.<number>)} output buffer. */
  this.output;
  /** @type {!number} output buffer pointer. */
  this.op = 0;
  /** @type {number} block pointer. */
  this.bp = 0;
  /** @type {number} block index. */
  this.bi = 0;
  /** @type {boolean} is final block flag. */
  this.bfinal = false;
  /** @type {boolean} resize flag for memory size optimization. */
  this.resize = false;

  // option parameters
  if (opt_params) {
    if (opt_params['index']) {
      this.ip = opt_params['index'];
    }
    if (opt_params['blockSize']) {
      this.blockSize = opt_params['blockSize'];
    }
  }

  this.output = new (USE_TYPEDARRAY ? Uint8Array : Array)(this.blockSize);
};

/**
 * decompress.
 * @return {!(Uint8Array|Array.<number>)} inflated buffer.
 */
ZlibStat.RawInflate.prototype.decompress = function() {
  /** @type {number} */
  var ip;

  // shortcut
  var block = this.block;
  var bp = this.bp;

  if (!USE_TYPEDARRAY) {
    this.output.subarray = this.output.slice;
  }

  while (!this.bfinal) {
    block[this.bi] = {};
    ip = this.ip;

    this.parseBlock();

    // save block data
    block[this.bi]['plain'] = this.output.subarray(bp, this.op);
    block[this.bi]['compressed'] = USE_TYPEDARRAY ?
      this.input.subarray(ip, this.ip) : this.input.slice(ip, this.ip);

    // next block
    ++this.bi;
    bp = this.op;
  }

  this.bp = bp;

  return this.concatBuffer();
};

/**
 * @const
 * @type {number} max backward length for LZ77.
 */
ZlibStat.RawInflate.MaxBackwardLength = 32768;

/**
 * @const
 * @type {number} max copy length for LZ77.
 */
ZlibStat.RawInflate.MaxCopyLength = 258;

/**
 * huffman order
 * @const
 * @type {!(Array.<number>|Uint8Array)}
 */
ZlibStat.RawInflate.Order = (function(table) {
  return USE_TYPEDARRAY ? new Uint16Array(table) : table;
})([16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]);

/**
 * huffman length code table.
 * @const
 * @type {!(Array.<number>|Uint16Array)}
 */
ZlibStat.RawInflate.LengthCodeTable = (function(table) {
  return USE_TYPEDARRAY ? new Uint16Array(table) : table;
})([
  0x0003, 0x0004, 0x0005, 0x0006, 0x0007, 0x0008, 0x0009, 0x000a, 0x000b,
  0x000d, 0x000f, 0x0011, 0x0013, 0x0017, 0x001b, 0x001f, 0x0023, 0x002b,
  0x0033, 0x003b, 0x0043, 0x0053, 0x0063, 0x0073, 0x0083, 0x00a3, 0x00c3,
  0x00e3, 0x0102, 0x0102, 0x0102
]);

/**
 * huffman length extra-bits table.
 * @const
 * @type {!(Array.<number>|Uint8Array)}
 */
ZlibStat.RawInflate.LengthExtraTable = (function(table) {
  return USE_TYPEDARRAY ? new Uint8Array(table) : table;
})([
  0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5,
  5, 5, 0, 0, 0
]);

/**
 * huffman dist code table.
 * @const
 * @type {!(Array.<number>|Uint16Array)}
 */
ZlibStat.RawInflate.DistCodeTable = (function(table) {
  return USE_TYPEDARRAY ? new Uint16Array(table) : table;
})([
  0x0001, 0x0002, 0x0003, 0x0004, 0x0005, 0x0007, 0x0009, 0x000d, 0x0011,
  0x0019, 0x0021, 0x0031, 0x0041, 0x0061, 0x0081, 0x00c1, 0x0101, 0x0181,
  0x0201, 0x0301, 0x0401, 0x0601, 0x0801, 0x0c01, 0x1001, 0x1801, 0x2001,
  0x3001, 0x4001, 0x6001
]);

/**
 * huffman dist extra-bits table.
 * @const
 * @type {!(Array.<number>|Uint8Array)}
 */
ZlibStat.RawInflate.DistExtraTable = (function(table) {
  return USE_TYPEDARRAY ? new Uint8Array(table) : table;
})([
  0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11,
  11, 12, 12, 13, 13
]);

/**
 * fixed huffman length code table
 * @const
 * @type {!Array}
 */
ZlibStat.RawInflate.FixedLiteralLengthTable = (function(table) {
  return table;
})((function() {
  var lengths = new (USE_TYPEDARRAY ? Uint8Array : Array)(288);
  var i, il;

  for (i = 0, il = lengths.length; i < il; ++i) {
    lengths[i] =
      (i <= 143) ? 8 :
      (i <= 255) ? 9 :
      (i <= 279) ? 7 :
      8;
  }

  return buildHuffmanTable(lengths);
})());

/**
 * fixed huffman distance code table
 * @const
 * @type {!Array}
 */
ZlibStat.RawInflate.FixedDistanceTable = (function(table) {
  return table;
})((function() {
  var lengths = new (USE_TYPEDARRAY ? Uint8Array : Array)(30);
  var i, il;

  for (i = 0, il = lengths.length; i < il; ++i) {
    lengths[i] = 5;
  }

  return buildHuffmanTable(lengths);
})());

/**
 * parse deflated block.
 */
ZlibStat.RawInflate.prototype.parseBlock = function() {
  /** @type {number} header */
  var hdr = this.readBits(3);

  // BFINAL
  if (hdr & 0x1) {
    this.bfinal = true;
  }
  this.block[this.bi]['final'] = hdr & 0x1;

  // BTYPE
  hdr >>>= 1;
  this.block[this.bi]['type'] = hdr;

  switch (hdr) {
    // uncompressed
    case 0:
      this.parseUncompressedBlock();
      break;
    // fixed huffman
    case 1:
      this.parseFixedHuffmanBlock();
      break;
    // dynamic huffman
    case 2:
      this.parseDynamicHuffmanBlock();
      break;
    // reserved or other
    default:
      throw new Error('unknown BTYPE: ' + hdr);
  }
};

/**
 * read inflate bits
 * @param {number} length bits length.
 * @return {number} read bits.
 */
ZlibStat.RawInflate.prototype.readBits = function(length) {
  var bitsbuf = this.bitsbuf;
  var bitsbuflen = this.bitsbuflen;
  var input = this.input;
  var ip = this.ip;

  /** @type {number} input and output byte. */
  var octet;

  // not enough buffer
  while (bitsbuflen < length) {
    // input byte
    octet = input[ip++];
    if (octet === void 0) {
      throw new Error('input buffer is broken');
    }

    // concat octet
    bitsbuf |= octet << bitsbuflen;
    bitsbuflen += 8;
  }

  // output byte
  octet = bitsbuf & /* MASK */ ((1 << length) - 1);
  bitsbuf >>>= length;
  bitsbuflen -= length;

  this.bitsbuf = bitsbuf;
  this.bitsbuflen = bitsbuflen;
  this.ip = ip;

  return octet;
};

/**
 * read huffman code using table
 * @param {Array} table huffman code table.
 * @return {number} huffman code.
 */
ZlibStat.RawInflate.prototype.readCodeByTable = function(table) {
  return this.readCodeByTablePlain(table) & 0xffff;
};

/**
 * read huffman code using table
 * @param {Array} table huffman code table.
 * @return {number} huffman code.
 */
ZlibStat.RawInflate.prototype.readCodeByTablePlain = function(table) {
  var bitsbuf = this.bitsbuf;
  var bitsbuflen = this.bitsbuflen;
  var input = this.input;
  var ip = this.ip;

  /** @type {!(Array.<number>|Uint8Array)} huffman code table */
  var codeTable = table[0];
  /** @type {number} */
  var maxCodeLength = table[1];
  /** @type {number} input byte */
  var octet;
  /** @type {number} code */
  var code;
  /** @type {number} code length & code (16bit, 16bit) */
  var codeWithLength;
  /** @type {number} code bits length */
  var codeLength;

  // not enough buffer
  while (bitsbuflen < maxCodeLength) {
    octet = input[ip++];
    if (octet === void 0) {
      throw new Error('input buffer is broken');
    }
    bitsbuf |= octet << bitsbuflen;
    bitsbuflen += 8;
  }

  // read max length
  codeWithLength = codeTable[bitsbuf & ((1 << maxCodeLength) - 1)];
  codeLength = codeWithLength >>> 16;

  this.bitsbuf = bitsbuf >> codeLength;
  this.bitsbuflen = bitsbuflen - codeLength;
  this.ip = ip;

  return codeWithLength;
};

/**
 * parse uncompressed block.
 */
ZlibStat.RawInflate.prototype.parseUncompressedBlock = function() {
  var input = this.input;
  var ip = this.ip;
  var output = this.output;
  var op = this.op;

  /** @type {number} input byte. */
  var octet;
  /** @type {number} block length */
  var len;
  /** @type {number} number for check block length */
  var nlen;
  /** @type {number} output buffer length */
  var olength = output.length;
  /** @type {number} copy counter */
  var preCopy;

  // skip buffered header bits
  this.bitsbuf = 0;
  this.bitsbuflen = 0;

  // len (1st)
  octet = input[ip++];
  if (octet === void 0) {
    throw new Error('invalid uncompressed block header: LEN (first byte)');
  }
  len = octet;

  // len (2nd)
  octet = input[ip++];
  if (octet === void 0) {
    throw new Error('invalid uncompressed block header: LEN (second byte)');
  }
  len |= octet << 8;

  // nlen (1st)
  octet = input[ip++];
  if (octet === void 0) {
    throw new Error('invalid uncompressed block header: NLEN (first byte)');
  }
  nlen = octet;

  // nlen (2nd)
  octet = input[ip++];
  if (octet === void 0) {
    throw new Error('invalid uncompressed block header: NLEN (second byte)');
  }
  nlen |= octet << 8;

  // check len & nlen
  if (len === ~nlen) {
    throw new Error('invalid uncompressed block header: length verify');
  }

  // check size
  if (ip + len > input.length) { throw new Error('input buffer is broken'); }

  // expand buffer
  while (op + len > output.length) {
    output = this.expandBuffer({fixRatio: 2});
  }

  // copy
  if (USE_TYPEDARRAY) {
    output.set(input.subarray(ip, ip + len), op);
    op += len;
    ip += len;
  } else {
    while (len--) {
      output[op++] = input[ip++];
    }
  }

  this.ip = ip;
  this.op = op;
  this.output = output;
};

/**
 * parse fixed huffman block.
 */
ZlibStat.RawInflate.prototype.parseFixedHuffmanBlock = function() {
  this.decodeHuffman(
    ZlibStat.RawInflate.FixedLiteralLengthTable,
    ZlibStat.RawInflate.FixedDistanceTable
  );
};

/**
 * parse dynamic huffman block.
 */
ZlibStat.RawInflate.prototype.parseDynamicHuffmanBlock = function() {
  /** @type {number} number of literal and length codes. */
  var hlit = this.readBits(5) + 257;
  /** @type {number} number of distance codes. */
  var hdist = this.readBits(5) + 1;
  /** @type {number} number of code lengths. */
  var hclen = this.readBits(4) + 4;
  /** @type {!(Uint8Array|Array.<number>)} code lengths. */
  var codeLengths =
    new (USE_TYPEDARRAY ? Uint8Array : Array)(ZlibStat.RawInflate.Order.length);
  /** @type {!Array} code lengths table. */
  var codeLengthsTable;
  /** @type {!(Uint32Array|Array.<number>)} literal and length code lengths. */
  var litlenLengths;
  /** @type {!(Uint32Array|Array.<number>)} distance code lengths. */
  var distLengths;
  /** @type {number} loop counter. */
  var i = 0;
  /** @type {number} loop counter. */
  var j = 0;

  // decode code lengths
  for (i = 0; i < hclen; ++i) {
    codeLengths[ZlibStat.RawInflate.Order[i]] = this.readBits(3);
  }
  codeLengthsTable = buildHuffmanTable(codeLengths);

  // decode function
  function decode(num, table, lengths) {
    var code;
    var prev;
    var repeat;
    var i = 0;

    for (i = 0; i < num;) {
      code = this.readCodeByTable(table);
      switch (code) {
        case 16:
          repeat = 3 + this.readBits(2);
          while (repeat--) { lengths[i++] = prev; }
          break;
        case 17:
          repeat = 3 + this.readBits(3);
          while (repeat--) { lengths[i++] = 0; }
          prev = 0;
          break;
        case 18:
          repeat = 11 + this.readBits(7);
          while (repeat--) { lengths[i++] = 0; }
          prev = 0;
          break;
        default:
          lengths[i++] = code;
          prev = code;
          break;
      }
    }

    return lengths;
  }

  // literal and length code
  litlenLengths = new (USE_TYPEDARRAY ? Uint8Array : Array)(hlit);

  // distance code
  distLengths = new (USE_TYPEDARRAY ? Uint8Array : Array)(hdist);

  //return;
  this.decodeHuffman(
    buildHuffmanTable(decode.call(this, hlit, codeLengthsTable, litlenLengths)),
    buildHuffmanTable(decode.call(this, hdist, codeLengthsTable, distLengths))
  );
};

/**
 * decode huffman code (dynamic)
 * @param {!Array} litlen literal and length code table.
 * @param {!Array} dist distination code table.
 */
ZlibStat.RawInflate.prototype.decodeHuffman = function(litlen, dist) {
  var output = this.output;
  var op = this.op;
  var block = this.block[this.bi];

  this.currentLitlenTable = litlen;
  this.currentDistTable = dist;

  block['litlenHuffmanTable'] = litlen[3];
  block['distHuffmanTable'] = dist[3];

  /** @type {number} output position limit. */
  var olength = output.length;
  /** @type {number} huffman code. */
  var code;
  /** @type {number} table index. */
  var ti;
  /** @type {number} huffman code distination. */
  var codeDist;
  /** @type {number} huffman code length. */
  var codeLength;
  /** @type {number} buffer position. */
  var bpos;
  /** @type {number} pre-copy counter. */
  var preCopy;
  /** @type {number} */
  var i;

  var blodk = this.block[this.bi];

  // LZSS buffer
  var lzss = block['lzss'] = [];
  var li = 0;
  var lzssCode = block['lzssCode'] = [];

  // statistics
  var stats = block['litlenCount'] =
    new (USE_TYPEDARRAY ? Uint32Array : Array)(288);

  // initialize
  if (!USE_TYPEDARRAY) {
    for (i = 0; i < 288; ++i) {
      stats[i] = 0;
    }
  }

  // litlen code length
  block['litlenCodeLength'] = 0;;

  while (true) {
    code = this.readCodeByTablePlain(litlen)
    block['litlenCodeLength'] += (code >>> 16);
    code = code & 0xffff;
    ++stats[code];

    // end of block
    if (code === 256) {
      break;
    }

    // literal
    if (code < 256) {
      lzss[li++] = code;

      if (op === olength) {
        output = this.expandBuffer();
        olength = output.length;
      }
      output[op++] = code;

      continue;
    }

    // length code
    ti = code - 257;
    codeLength = ZlibStat.RawInflate.LengthCodeTable[ti];
    if (ZlibStat.RawInflate.LengthExtraTable[ti] > 0) {
      codeLength += this.readBits(ZlibStat.RawInflate.LengthExtraTable[ti]);
    }

    // dist code
    code = this.readCodeByTable(dist);
    codeDist = ZlibStat.RawInflate.DistCodeTable[code];
    if (ZlibStat.RawInflate.DistExtraTable[code] > 0) {
      codeDist += this.readBits(ZlibStat.RawInflate.DistExtraTable[code]);
    }

    // lzss object
    lzssCode.push(lzss[li++] = new LZSS(codeLength, codeDist));

    // lz77 decode
    if (op + codeLength >= olength) {
      output = this.expandBuffer();
      olength = output.length;
    }
    while (codeLength--) {
      output[op] = output[(op++) - codeDist];
    }
  }

  while (this.bitsbuflen >= 8) {
    this.bitsbuflen -= 8;
    this.ip--;
  }

  this.op = op;
};

/**
 * @constructor
 * @param {number} length length.
 * @param {number} distance distance.
 */
function LZSS(length, distance) {
  this.length = length;
  this.distance = distance;
}

/**
 * expand output buffer. (dynamic)
 * @param {Object=} opt_param option parameters.
 * @return {!(Array.<number>|Uint8Array)} output buffer pointer.
 */
ZlibStat.RawInflate.prototype.expandBuffer = function(opt_param) {
  /** @type {!(Array.<number>|Uint8Array)} store buffer. */
  var buffer;
  /** @type {number} expantion ratio. */
  var ratio = (this.input.length / this.ip + 1) | 0;
  /** @type {number} maximum number of huffman code. */
  var maxHuffCode;
  /** @type {number} new output buffer size. */
  var newSize;
  /** @type {number} max inflate size. */
  var maxInflateSize;

  var input = this.input;
  var output = this.output;

  if (opt_param) {
    if (typeof opt_param.fixRatio === 'number') {
      ratio = opt_param.fixRatio;
    }
    if (typeof opt_param.addRatio === 'number') {
      ratio += opt_param.addRatio;
    }
  }

  // calculate new buffer size
  if (ratio < 2) {
    maxHuffCode =
      (input.length - this.ip) / this.currentLitlenTable[2];
    maxInflateSize = (maxHuffCode / 2 * 258) | 0;
    newSize = maxInflateSize < output.length ?
      output.length + maxInflateSize :
      output.length << 1;
  } else {
    newSize = output.length * ratio;
  }

  // buffer expantion
  if (USE_TYPEDARRAY) {
    buffer = new Uint8Array(newSize);
    buffer.set(output);
  } else {
    buffer = output;
  }

  this.output = buffer;

  return this.output;
};

/**
 * concat output buffer. (dynamic)
 * @return {!(Array.<number>|Uint8Array)} output buffer.
 */
ZlibStat.RawInflate.prototype.concatBuffer = function() {
  /** @type {Array.<number>|Uint8Array} output buffer. */
  var buffer;
  var resize = this.resize;

  var op = this.op;

  if (resize) {
    if (USE_TYPEDARRAY) {
      buffer = new Uint8Array(op);
      buffer.set(this.output.subarray(0, op));
    } else {
      buffer = this.output.slice(0, op);
    }
  } else {
    buffer =
      USE_TYPEDARRAY ?  this.output.subarray(0, op) : this.output.slice(0, op);
  }


  this.buffer = buffer;

  return this.buffer;
};


//*****************************************************************************
// export
//*****************************************************************************
if (ZLIBSTAT_RAW_INFLATE_EXPORT) {
  goog.exportSymbol('ZlibStat.RawInflate', ZlibStat.RawInflate);
  goog.exportSymbol(
    'ZlibStat.RawInflate.prototype.decompress',
    ZlibStat.RawInflate.prototype.decompress
  );
}


// end of scope
});

/* vim:set expandtab ts=2 sw=2 tw=80: */
