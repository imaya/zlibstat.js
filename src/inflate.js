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

goog.provide('ZlibStat.Inflate');

//-----------------------------------------------------------------------------

/** @define {boolean} export symbols. */
var ZLIBSTAT_INFLATE_EXPORT = false;

//-----------------------------------------------------------------------------

goog.require('ZlibStat.Enum');
goog.require('ZlibStat.RawInflate');
goog.require('Zlib.Adler32');

goog.scope(function() {

/**
 * @constructor
 * @param {!(Uint8Array|Array)} input deflated buffer.
 * @param {Object=} opt_params option parameters.
 *
 * opt_params は以下のプロパティを指定する事ができます。
 *   - index: input buffer の deflate コンテナの開始位置.
 *   - blockSize: バッファのブロックサイズ.
 *   - verify: 伸張が終わった後 adler-32 checksum の検証を行うか.
 */
ZlibStat.Inflate = function(input, opt_params) {
  /** @type {number} */
  var blockSize;
  /** @type {number} */
  var cmf;
  /** @type {number} */
  var flg;

  /** @type {!(Uint8Array|Array)} */
  this.input = input;
  /** @type {number} */
  this.ip = 0;
  /** @type {ZlibStat.RawInflate} */
  this.rawinflate;
  /** @type {(boolean|undefined)} verify flag. */
  this.verify;

  // option parameters
  if (opt_params) {
    if (opt_params.index) {
      this.ip = opt_params.index;
    }
    if (opt_params.blockSize) {
      blockSize = opt_params.blockSize;
    }
    if (opt_params.verify) {
      this.verify = opt_params.verify;
    }
  }

  // Compression Method and Flags
  cmf = input[this.ip++];
  flg = input[this.ip++];

  // compression method
  switch (cmf & 0x0f) {
    case ZlibStat.CompressionMethod.DEFLATE:
      this.method = ZlibStat.CompressionMethod.DEFLATE;
      break;
    default:
      throw new Error('unsupported compression method');
  }

  // fcheck
  if (((cmf << 8) + flg) % 31 !== 0) {
    throw new Error('invalid fcheck flag:' + ((cmf << 8) + flg) % 31);
  }

  // fdict (not supported)
  if (flg & 0x20) {
    throw new Error('fdict flag is not supported');
  }

  // RawInflate
  this.rawinflate = new ZlibStat.RawInflate(input, {
    index: this.ip,
    blockSize: blockSize
  });
}

/**
 * decompress.
 * @return {!(Uint8Array|Array)} inflated buffer.
 */
ZlibStat.Inflate.prototype.decompress = function() {
  /** @type {!(Array|Uint8Array)} input buffer. */
  var input = this.input;
  /** @type {!(Uint8Array|Array)} inflated buffer. */
  var buffer;
  /** @type {number} adler-32 checksum */
  var adler32;

  buffer = this.rawinflate.decompress();
  this.ip = this.rawinflate.ip;

  // verify adler-32
  adler32 = (
    (input[this.ip++] << 24) | (input[this.ip++] << 16) |
    (input[this.ip++] <<  8) | (input[this.ip++])
  ) >>> 0;

  if (adler32 !== Zlib.Adler32(buffer)) {
    throw new Error('invalid adler-32 checksum');
  }

  return buffer;
};

/**
 * get deflate blocks.
 * @return {!Array} deflate blocks.
 */
ZlibStat.Inflate.prototype.getBlocks = function() {
  return this.rawinflate.block;
};

//*****************************************************************************
// export
//*****************************************************************************
if (ZLIBSTAT_INFLATE_EXPORT) {
  goog.exportSymbol('ZlibStat.Inflate', ZlibStat.Inflate);
  goog.exportSymbol(
    'ZlibStat.Inflate.prototype.decompress',
    ZlibStat.Inflate.prototype.decompress
  );
  goog.exportSymbol(
    'ZlibStat.Inflate.prototype.getBlocks',
    ZlibStat.Inflate.prototype.getBlocks
  );
}


// end of scope
});

/* vim:set expandtab ts=2 sw=2 tw=80: */
