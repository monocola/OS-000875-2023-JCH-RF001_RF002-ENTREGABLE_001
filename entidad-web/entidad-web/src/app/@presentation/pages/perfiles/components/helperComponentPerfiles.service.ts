import { Injectable } from '@angular/core';
import { DetalleMaestra } from 'src/app/@data/model/detalleMaestra';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';
import { PerfilesRepository } from 'src/app/@domain/repository/perfiles.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { forkJoin } from 'rxjs';

@Injectable({
  providedIn: 'root',
})
export class HelperLeyComponentsPerfilesService {
  dataFetched = false;

  carrerasToShow = [];

  nivelesEducativos: DetalleMaestra[] = [];
  estadosNiveles: DetalleMaestra[] = [];
  grados: DetalleMaestra[] = [];
  estadosGrado: DetalleMaestra[] = [];
  carreras = [];

  constructor(
    private toastService: ToastService,
    private maestraService: MaestraRepository,
    private perfilesService: PerfilesRepository
  ) {
    this.loadCombox();
  }

  getActive(array: any[]) {
    return array.filter((el) => el.estadoRegistro === '1');
  }

  loadCombox() {
    const getNivelesEducativos = this.maestraService.getMaestraDetalleByCod(
      'TBL_PER_NIV_EDU'
    );
    const getNivelesEstados = this.maestraService.getMaestraDetalleByCod(
      'TBL_PER_EST_NIV_EDU'
    );
    const getGrados = this.maestraService.getMaestraDetalleByCod(
      'TBL_PER_SIT_ACA'
    );
    const getEstadoGrados = this.maestraService.getMaestraDetalleByCod(
      'TBL_PER_EST_GRA'
    );

    forkJoin([
      getNivelesEducativos,
      getNivelesEstados,
      getGrados,
      getEstadoGrados,
    ]).subscribe(
      (results) => {
        this.dataFetched = true;
        this.nivelesEducativos = this.getActive(results[0]);
        this.estadosNiveles = this.getActive(results[1]);
        this.grados = this.getActive(results[2]);
        this.estadosGrado = this.getActive(results[3]);
      },
      (err) => {
        this.dataFetched = false;
        this.toastService.showToast(err, 'danger');
      }
    );
  }

  getCarrerasById(id): Promise<any> {
    return new Promise((resolve, reject) =>
      this.perfilesService.getCarrerasBySituation(id).subscribe(
        (res) => {
          this.carreras = res.slice(0);
          this.carrerasToShow = res.slice(0);
          resolve(this.carreras);
        },
        (err) => {
          reject(err);
        }
      )
    );
  }

  getDetalle(type: string, maestras: any[]) {
    return (
      maestras.filter((m) => m.codigoCabecera === type)[0]
        .listMaestraDetalles || []
    );
  }
}
