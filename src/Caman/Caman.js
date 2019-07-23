const chunk = function(array, size) {
    if (!array.length) {
          return [];
        }
    const head = array.slice(0, size);
    const tail = array.slice(size);

    return [head, ...chunk(tail, size)];
};

var constructFn = function(c) {
  return function(i) {
    var fn = i[0];
    var args = i.slice(1).map(eval);

    if (fn === "channels") {
      c[fn].apply(c, [{ red: args[0], green: args[1], blue: args[2] }])
    } else if (fn === "resize") {
      c[fn].apply(c, { width: args[0], height: args[1] })
    } else {
      c[fn].apply(c, args);
    }
  }
}

exports.processKernelImpl = function(name) {
  return function(matrix) {
    return function() {
      var c = Caman.Filter.register(name, function() {
        this.processKernel(name + "K", matrix.map(parseInt));
      });
    }
  }
}

exports.registerImpl = function(name) {
  return function(psfn) {
    return function() {
      Caman.Filter.register(name, function(adjust) {
        this.process(name, function(rgba) {
          return psfn(this)(adjust)(rgba.r)(rgba.g)(rgba.b)();
        });
      });
    }
  }
}

exports.newLayerImpl = function(elid) {
  return function(layerOptions) {
    return function(filter) {
      return function(onError, onSuccess) {
        try {
          Caman(elid, function() {
            var c = this;

            c.newLayer(function() {
              var l = this;
              
              layerOptions.forEach(constructFn(l));
              filter.forEach(constructFn(l.filter));
            });

            c.render(onSuccess);
          });
        } catch (e) {
          onError(e)
        }
      }
    }
  }
}

exports.functionWrapper = function(elid) {
  return function(filter) {
    return function(onError, onSuccess) {
      try {
        Caman(elid, function() {
          var c = this;
          filter.forEach(constructFn(c));
          c.render(onSuccess);
        });
      } catch (e) {
        onError(e);
      }
    };
  };
};

exports.ppFunctionWrapper= function(pp) {
  return function(fn) {
    return function(args) {
      return function() {
        if(fn.indexOf("put") > 0) {
          pp[fn](args[0], args[1], { r: args[2], g: args[3], b: args[4], a: args[5] })
          return []
        } else {
          return pp[fn].apply(pp, args)
        }
      }
    }
  }
}
