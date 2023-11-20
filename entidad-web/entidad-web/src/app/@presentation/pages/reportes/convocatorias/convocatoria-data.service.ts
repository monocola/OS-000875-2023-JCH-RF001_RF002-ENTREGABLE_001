import { Injectable } from '@angular/core';
@Injectable({
  providedIn: 'root',
})
export class ConvocatoriaDataService {

  _convocatoriaId: number;
  _obj: any;


  set convocatoriaId(val: number) {
    this._convocatoriaId = val;
  }

  get convocatoriaId(): number {
    return this._convocatoriaId;
  }

  set objConv(val: any) {
    this._obj = val;
  }

  get objConv(): any {
    return this._obj;
  }

  constructor() {}

}
