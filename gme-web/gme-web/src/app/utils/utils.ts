import { DatePipe } from '@angular/common';
import moment from 'moment';
import { AbstractControl } from '@angular/forms';

export class Utils {
  static getKeysJson(columnsIn: {}): string[] {
    let result = [];

    for (const key of Object.keys(columnsIn)) {
      result.push(key);
    }
    return result;
  }
  static sortDate = (direction: any, a: string, b: string): number => {
    let first = Number(
      new DatePipe('es-PE').transform(a, 'YYYY-MM-DDTHH:mm:ss.sssZ')
    );
    let second = Number(
      new DatePipe('static').transform(b, 'YYYY-MM-DDTHH:mm:ss.sssZ')
    );

    if (first < second) {
      return -1 * direction;
    }
    if (first > second) {
      return direction;
    }
    return 0;
  }

  static parseDate(date: Date, format: string) {
    return date != null ? moment(date).format(format) : null;
  }

  static convertStringToDate(dateString: string): Date {
    if (dateString === null) return null;
    const year = Number.parseInt(dateString.substring(0, 4), 10);
    const month = Number.parseInt(dateString.substring(5, 7), 10);
    const day = Number.parseInt(dateString.substring(8, 10), 10);
    return new Date(year, month - 1, day);
  }

  static getfechaActual() {
    moment.locale('es');
    return moment().format('YYYY-MM-DD HH:mm:ss');
  }

  static getfechaFormatoEnvio(date: Date) {
    moment.locale('es');
    return moment(date).format('YYYY-MM-DD HH:mm:ss');
  }

  static getfechaActualMas1Year() {
    moment.locale('es');
    return moment().add(1, 'year').format('YYYY-MM-DD HH:mm:ss');
  }
  static arraysEqual(a1, a2) {
    return JSON.stringify(a1) === JSON.stringify(a2);
  }
  static formatFechaString(fecha, formato) {
    let date = new Date(fecha);
    moment.locale('es');
    return moment(date).format(formato);
  }

  static parseFechaString(dateString) {
    let dateMomentObject = moment(dateString, 'DD/MM/YYYY HH:mm:ss');
    return dateMomentObject.toDate();
  }

  static formatFechaDate(date, formato) {
    return moment(date).format(formato);
  }
  static notCorrectDataValidator(control: AbstractControl): { errorCorrectData: boolean } {
    let result = !control.get(['filtros']).value.map(item => item.descripcion).includes(control.get(['name']).value);
    if (result) {
      return { errorCorrectData : true};
    } else {
      return null;
    }

  }
}

export class ItemTable {
  codigo: string;
  descripcion: string;
  lote: string;
  um: string;
  ubicacion: string;
  cantidad: number;
}

export interface IBaseCallback {
  onSuccess(): void;
}
