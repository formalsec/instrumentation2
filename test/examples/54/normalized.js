const v1 = require('child_process');
var exec = v1.exec;
var utils = require('./utils');
const v60 = function (proto) {
    const compare = function (orig, compareTo, options, cb) {
        orig = utils.escape(orig);
        compareTo = utils.escape(compareTo);
        const v2 = this._options;
        const v3 = this._options;
        const v4 = v3.imageMagick;
        var isImageMagick = v2 && v4;
        let bin;
        if (isImageMagick) {
            bin = '';
        } else {
            bin = 'gm ';
        }
        const v5 = bin + 'compare -metric mse ';
        const v6 = v5 + orig;
        const v7 = v6 + ' ';
        var execCmd = v7 + compareTo;
        var tolerance = 0.4;
        const v8 = typeof options;
        const v9 = v8 === 'object';
        if (v9) {
            const v10 = options.highlightColor;
            const v11 = options.highlightColor;
            const v12 = v11.indexOf('"');
            const v13 = v12 < 0;
            const v14 = v10 && v13;
            if (v14) {
                const v15 = options.highlightColor;
                const v16 = '"' + v15;
                options.highlightColor = v16 + '"';
            }
            const v17 = options.file;
            if (v17) {
                const v18 = options.file;
                const v19 = typeof v18;
                const v20 = v19 !== 'string';
                if (v20) {
                    const v21 = new TypeError('The path for the diff output is invalid');
                    throw v21;
                }
                let highlightColorOption;
                const v22 = options.highlightColor;
                const v23 = options.highlightColor;
                const v24 = ' -highlight-color ' + v23;
                const v25 = v24 + ' ';
                if (v22) {
                    highlightColorOption = v25;
                } else {
                    highlightColorOption = ' ';
                }
                let highlightStyleOption;
                const v26 = options.highlightStyle;
                const v27 = options.highlightStyle;
                const v28 = ' -highlight-style ' + v27;
                const v29 = v28 + ' ';
                if (v26) {
                    highlightStyleOption = v29;
                } else {
                    highlightStyleOption = ' ';
                }
                const v30 = options.file;
                var diffFilename = utils.escape(v30);
                let diffOpt;
                const v31 = '-file ' + diffFilename;
                if (isImageMagick) {
                    diffOpt = diffFilename;
                } else {
                    diffOpt = v31;
                }
                const v32 = highlightColorOption + highlightStyleOption;
                const v33 = v32 + ' ';
                execCmd += v33 + diffOpt;
            }
            const v34 = options.tolerance;
            const v35 = typeof v34;
            const v36 = v35 != 'undefined';
            if (v36) {
                const v37 = options.tolerance;
                const v38 = typeof v37;
                const v39 = v38 !== 'number';
                if (v39) {
                    const v40 = new TypeError('The tolerance value should be a number');
                    throw v40;
                }
                tolerance = options.tolerance;
            }
        } else {
            const v41 = isImageMagick && (execCmd += ' null:');
            v41;
            const v42 = typeof options;
            const v43 = v42 == 'function';
            if (v43) {
                cb = options;
            } else {
                tolerance = options;
            }
        }
        const v58 = function (err, stdout, stderr) {
            if (isImageMagick) {
                const v44 = !err;
                if (v44) {
                    const v45 = 0 <= tolerance;
                    const v46 = cb(null, v45, 0, stdout);
                    return v46;
                }
                const v47 = err.code;
                const v48 = v47 === 1;
                if (v48) {
                    err = null;
                    stdout = stderr;
                }
            }
            if (err) {
                const v49 = cb(err);
                return v49;
            }
            let regex;
            if (isImageMagick) {
                regex = /\((\d+\.?[\d\-\+e]*)\)/m;
            } else {
                regex = /Total: (\d+\.?\d*)/m;
            }
            var match = regex.exec(stdout);
            const v50 = !match;
            if (v50) {
                const v51 = 'Unable to parse output.\nGot ' + stdout;
                err = new Error(v51);
                const v52 = cb(err);
                return v52;
            }
            const v53 = match[1];
            var equality = parseFloat(v53);
            const v54 = equality <= tolerance;
            const v55 = utils.unescape(orig);
            const v56 = utils.unescape(compareTo);
            const v57 = cb(null, v54, equality, stdout, v55, v56);
            v57;
        };
        const v59 = exec(execCmd, v58);
        v59;
    };
    if (proto) {
        proto.compare = compare;
    }
    return compare;
};
const v61 = exports;
module.exports = v61;