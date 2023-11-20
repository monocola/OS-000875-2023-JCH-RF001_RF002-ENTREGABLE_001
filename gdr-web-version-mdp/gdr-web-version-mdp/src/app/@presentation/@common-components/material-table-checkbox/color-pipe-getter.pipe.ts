import { Pipe, PipeTransform } from '@angular/core';

@Pipe({
  name: 'colorPipeGetter'
})
export class ColorPipeGetterPipe implements PipeTransform {

  transform(object: any, keyName: string, ...args: unknown[]): unknown {
    if (object.settings) {
      return object?.settings[keyName]?.color;
    } else {
      return 'black';
    }
  }

}
