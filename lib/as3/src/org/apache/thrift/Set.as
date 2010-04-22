package org.apache.thrift {
  import flash.utils.Dictionary;
  
  
  public class Set {
    
    private var _elements:Dictionary = new Dictionary();
    private var _size:int = 0;
    
    public function Set(... values) {
      for each (var value:* in values) {
        add(value);
      }
    }

    public function add(o:*):Boolean {
      var alreadyPresent:Boolean = _elements.hasOwnProperty(o);
      if (! alreadyPresent) {
        _size++;
        _elements[o] = true;
      }
     
      return ! alreadyPresent;
    }

    public function clear():void {
      for (var value:* in _elements) {
        remove(value);
      }
    }
    
    public function contains(o:Object):Boolean {
      return _elements.hasOwnProperty(o);
    }
    
    public function isEmpty():Boolean {
      return _size == 0;
    }
    
    public function remove(o:*):Boolean {
      if (contains(o)) {
        delete _elements[o];
        _size--;
        return true;
      }
      else {
        return false;
      }
    }
    
    public function toArray():Array {
      var ret:Array = new Array();
      for (var key:* in _elements) {
        ret.push(key);
      }
      return ret;
    }
    
    public function get size():int {
      return _size;
    }
  }
}