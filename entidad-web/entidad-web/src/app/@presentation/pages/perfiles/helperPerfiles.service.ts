import { Injectable } from '@angular/core';
import { forkJoin } from 'rxjs';
import { EvaluacionesServirRepository } from '../../../@domain/repository/evaluaciones-servir.repository';
import { UnidadOrganicaRepository } from 'src/app/@domain/repository/unidad-organica.repository';
import { OrganoRepository } from 'src/app/@domain/repository/organo.repository';
import { PerfilesRepository } from 'src/app/@domain/repository/perfiles.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { ParameterRepository } from 'src/app/@domain/repository/parameter.repository';
import { DetalleMaestra } from 'src/app/@data/model/detalleMaestra';

@Injectable()
export class HelperPerfilesService {
  constructor(
    private evaluacionServirService: EvaluacionesServirRepository,
    private unidadOrganicaRepository: UnidadOrganicaRepository,
    private parametrosRepository: ParameterRepository,
    private perfilesRepository: PerfilesRepository,
    private organoRepository: OrganoRepository,
    private toastService: ToastService
  ) {}

  regimenes: DetalleMaestra[] = [];
  organos = [];
  estados = [];
  unidadesOrganicas = [];
  regimenesToCreate: DetalleMaestra[] = [];

  loadCombox() {
    if (this.organos.length === 0 && this.regimenes.length === 0) {
      const getRegimenes = this.evaluacionServirService.getRegimenesServir('TBL_REGIMEN');
      const getUnidadesOrganicas = this.unidadOrganicaRepository.getUnidadesOrganicas(
        true
      );
      const getOrganos = this.organoRepository.getOrganos(true);
      const getRemigenesToCreate = this.perfilesRepository.getRegimenesToCreate();
      const getEstados = this.parametrosRepository.getEstadoRegistro();

      forkJoin([
        getRemigenesToCreate,
        getRegimenes,
        getUnidadesOrganicas,
        getOrganos,
        getEstados,
      ]).subscribe(
        (results) => {
          this.regimenesToCreate = results[0];
          this.regimenes = results[1];
          this.unidadesOrganicas = results[2];
          this.organos = results[3];
          this.estados = results[4];
        },
        (err) => {
          this.toastService.showToast(err, 'danger');
        }
      );
    }
  }

  getRegimenesToCreate() {
    if (this.regimenesToCreate.length === 0) {
      this.perfilesRepository.getRegimenesToCreate().subscribe(
        (res) => {
          this.regimenesToCreate = res;
        },
        (err) => {
          this.toastService.showToast(err, 'danger');
        }
      );
    }
  }

  initializeValues() {
    this.regimenes = [];
    this.organos = [];
    this.unidadesOrganicas = [];
    this.regimenesToCreate = [];
  }
}
