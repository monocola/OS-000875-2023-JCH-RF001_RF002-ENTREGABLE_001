import { Pipe, PipeTransform } from '@angular/core';

@Pipe({
  name: 'fillDigit',
})
export class FillDigitPipe implements PipeTransform {
  transform(value: any, ...args: unknown[]): unknown {
    console.log(value.toString());
    if (value.toString().length === 1) {
      return '0' + value;
    }
    return null;
  }
}
