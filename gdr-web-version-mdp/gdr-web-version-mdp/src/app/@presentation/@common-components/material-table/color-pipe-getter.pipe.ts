import { Pipe, PipeTransform } from '@angular/core';

@Pipe({
  name: 'colorPipeGetter'
})
export class ColorPipeGetterPipe implements PipeTransform {

  transform(object: any, keyName: string, ...args: unknown[]): unknown {
    if (object[keyName] === "1") {
      return '#0DACBC';
    } else {
      return 'black';
    }
  }
}
