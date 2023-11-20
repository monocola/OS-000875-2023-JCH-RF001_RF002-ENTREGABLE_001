import { Injectable } from '@angular/core';
import { forkJoin } from 'rxjs';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';

@Injectable({
  providedIn: 'root',
})
export class CreacionFormBaseService {
  tipoInforme = null;

  dataToEdit = null;
  informeIdToEdit = null;

  tipoBonificaciones = [];
  nivelesBonificaciones = [];
  aplicaSobre = [];

  tiposBonificacionesSelected = [];

  constructor(private maestraService: MaestraRepository) {}

  loadData() {
    const getBonificaciones = this.maestraService.getMaestraDetalleByCod(
      'TIP_BON'
    );
    const getNivelesBonif = this.maestraService.getMaestraDetalleByCod(
      'TBL_BONI_NIVEL'
    );
    const getAplicaBonif = this.maestraService.getMaestraDetalleByCod(
      'TBL_BONI_APLICA'
    );
    forkJoin([getBonificaciones, getNivelesBonif, getAplicaBonif]).subscribe(
      (results) => {
        this.tipoBonificaciones = results[0];
        this.nivelesBonificaciones = results[1];
        this.aplicaSobre = results[2];
      },
      (err) => {}
    );
  }

  initializeValues() {
    this.informeIdToEdit = null;
    this.tipoInforme = null;
    this.dataToEdit = null;
    this.tiposBonificacionesSelected = [];
    sessionStorage.removeItem('tipoInforme');
  }

  getValues() {}
}
