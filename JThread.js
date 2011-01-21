/**
 * Summary:
 *      Most of the current browsers run all javascript functions/loops within a
 *      single thread causing the browser to freeze while executing a long
 *      running loop or function, and may cause the browser to display the
 *      "Unresponsive script" message. JThread attempts to solve those issues.
 *      JThread has the following features: -
 *          1- Doesn't freeze the browser while running a loop.
 *          2- Allows multiple threads to run concurrently.
 *          3- Can be killed at any time.
 *
 * Example 1:
 *      var jThread = new JThread(loopBody, exitCondition);//instantiate
 *      ...
 *      jThread.run();//run
 *      ...
 *      jThread.kill();//kill
 *      ...
 *      ...
 *      var jThread = new JThread(loopBody, exitCondition).run();//instantiate and run
 *      ...
 *      jThread.kill();//kill
 *      ...
 *
 * Example 2:
 *      var bigArr = [];
 *      var jThread = JThread.forEach(bigArr, callback);//instantiate and run
 *      ...
 *      jThread.kill();//kill
 *
 * Drawbacks:
 *      Slower than regular loops. But this can be controlled byt "_freezeIter"
 *      variable that controls the number of iterations to be executed during
 *      which the browser will freeze.
 *      
 */

/**
 * JThread arguments: -
 * 
 *      loopBody: (Function)
 *          A function to be executed every time the "exitCondition" returns
 *          true. Takes one argument:
 *              jThread: The current running jThread object.
 *
 *      exitCondition: (Function)
 *          A function to be executed to return false to exit the loop, or
 *          true to execute another iteration of the loop. Takes one argument:
 *              jThread: The current running jThread object.
 *
 *      onEnd: (Function - optional)
 *          A function to be executed when ending the loop normally. Takes one
 *          argument:
 *              jThread: The current running jThread object.
 *
 *      onKill: (Function - optional)
 *          A function to be executed when "kill" function is called to end the
 *          loop. Takes one argument:
 *              jThread: The current running jThread object.
 *
 *      scope: (Object - optional)
 *          A scoping object. The scope in which the functions will be executed.
 *
 */
var JThread = function (/*Function*/loopBody, /*Function*/exitCondition, /*[Function]*/onEnd, /*[Function]*/onKill, /*[Object]*/scope){
    //private attributes
    var _threadId = new Date().getTime() + ((++JThread.id) % 1000),
    _freezeIter = 10,
    _kill = false,
    _killed = false,
    _running = false,
    _sTime, _eTime;
    
    scope = scope?scope:window;
    
    //private functions
    var _reset = function(){
        _kill = false;
        _killed = false;
        _running = false;
    };

    return {
        //public functions
        getThreadId: function(){
            return _threadId;
        },
        isRunning: function(){
            return _running;
        },
        isKilled: function(){
            return _killed;
        },
        getTime: function(){
            return (_eTime.getTime() - _sTime.getTime())/1000;
        },
        kill: function(){
            _kill = true;
        },
        run: function(){
            var jThread = this;
            _sTime = new Date();
            _reset();
            var f = function(){
                if(_kill){
                    _killed = true;
                    _running = false;
                    _eTime = new Date();
                    if(onKill){
                        onKill.apply(scope,[jThread]);
                    }
                    return;
                }
                var i = _freezeIter;
                while(i--){
                    if(exitCondition.apply(scope,[jThread])){
                        _running = true;
                        loopBody.apply(scope,[jThread]);
                        if(!i){
                            setTimeout(f,0);
                        }
                    }else{
                        _running = false;
                        _eTime = new Date();
                        if(onEnd){
                            onEnd.apply(scope,[jThread]);
                        }
                        break;
                    }
                }
            };
            f();
            return jThread;
        }
    };
};

/**
 * JThread.forEach arguments: -
 *
 *      arr: (Array)
 *          An array to be traversed.
 *
 *      callback: (Function)
 *          A function to be executed with every iteration while traversing an
 *          array. Takes three arguments:
 *              jThread: The current running jThread object.
 *              element: The current array element.
 *              index: The current array index.
 *
 *      onEnd: (Function - optional)
 *          A function to be executed when ending the loop normally. Takes one
 *          argument:
 *              jThread: The current running jThread object.
 *
 *      onKill: (Function - optional)
 *          A function to be executed when "kill" function is called to end the
 *          loop. Takes one argument:
 *              jThread: The current running jThread object.
 *
 *      scope: (Object - optional)
 *          A scoping object. The scope in which the functions will be executed.
 *
 */
JThread.forEach = function(/*Array*/arr, /*Function*/callback, /*[Function]*/onEnd, /*[Function]*/onKill, /*[Object]*/scope){
    arr = arr?(arr instanceof Array?arr:[arr]):[];
    var i = 0;
    var loopBody = function(t){
        callback.apply(scope, [/*jThread*/t, /*element*/arr[i], /*index*/i++]);
    }
    var exitCondition = function(t){
        return i<arr.length;
    }
    return new JThread(loopBody, exitCondition, onEnd, onKill, scope).run();
};

JThread.id = 0;