import { Injectable } from '@angular/core';
import { forkJoin } from 'rxjs';
import { DetalleMaestra } from 'src/app/@data/model/detalleMaestra';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';
import { ParameterRepository } from 'src/app/@domain/repository/parameter.repository';

@Injectable({
  providedIn: 'root',
})
export class BonificacionesHelperService {
  tiposBonificacion = [];
  estados = [];
  niveles: DetalleMaestra[] = [];
  aplicaSobre: DetalleMaestra[] = [];
  bonificacionSelected: DetalleMaestra = null;

  constructor(
    private maestraService: MaestraRepository,
    private parametrosService: ParameterRepository
  ) {}

  loadCombox() {
    if (this.tiposBonificacion.length !== 0) return;
    const getEstados = this.parametrosService.getEstadoRegistro();
    const getBonificaciones = this.maestraService.getMaestraDetalleByCod(
      'TIP_BON'
    );
    const getNivelesBonif = this.maestraService.getMaestraDetalleByCod(
      'TBL_BONI_NIVEL'
    );
    const getAplicaBonif = this.maestraService.getMaestraDetalleByCod(
      'TBL_BONI_APLICA'
    );
    forkJoin([
      getBonificaciones,
      getEstados,
      getNivelesBonif,
      getAplicaBonif,
    ]).subscribe((results) => {
      this.tiposBonificacion = results[0];
      this.estados = results[1];
      this.niveles = results[2];
      this.aplicaSobre = results[3];
    });
  }
}
