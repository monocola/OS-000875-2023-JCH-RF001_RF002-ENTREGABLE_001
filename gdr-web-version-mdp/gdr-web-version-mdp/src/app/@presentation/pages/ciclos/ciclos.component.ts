import { Component, OnInit } from '@angular/core';
import { TableColumn } from '../../@common-components/material-table/table-column';
import { Sort } from '@angular/material/sort';
import { sortDataTableComponent } from '../../../utils/general';
import { Ciclos } from '../../../@data/model/ciclos';
import { RegistrarCicloComponent } from './registrar-ciclo/registrar-ciclo.component';
import { ToastService } from '../../@common-components/toast';
import { MatDialog } from '@angular/material/dialog';
import { CicloService } from '../../../@data/services/ciclo.service';
import { forkJoin } from 'rxjs';
import { AuthenticationRepository } from '../../../@domain/repository/authentication.repository';
import { DatePipe } from '@angular/common';
import { MaestraParametroRepository } from '../../../@domain/repository/maestra-parametro.repository';
import { MaestraParametro } from '../../../@data/model/maestra-parametro';
import { Utils } from 'src/app/utils/utils';

@Component({
  selector: 'serv-talento-ciclos',
  templateUrl: './ciclos.component.html',
  styleUrls: ['./ciclos.component.scss'],
})
export class CiclosComponent implements OnInit {
  constructor(
    private toast: ToastService,
    private dialog: MatDialog,
    private cicloService: CicloService,
    private authenticationService: AuthenticationRepository,
    private datePipe: DatePipe,
    private maeParametroRepository: MaestraParametroRepository,
    private toastService: ToastService
  ) {}

  isMonitoreo:boolean = false;
  ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
  profile = this.authenticationService.getCurrentUserValue;
  holderText = 'Buscar por Año ciclo GDR, Fecha incio, Fecha fin, Cronograma institucional, Estado';
  lstCiclos: Ciclos[] = [];
  ordersTableColumns: TableColumn[];
  dataToEdit: Ciclos = null;
  dataToDelete: Ciclos = null;
  estados: MaestraParametro[];
  cicloDefault: number;
  cicloDefaultDesc: string;
  anio: number;

  ngOnInit(): void {
    this.loadCombox();
    this.initializeColumns();
  }
 
  Monitoreo() {
    let monitoreo = Utils.obtenerSiEsPantallMonitoreo();
    if(monitoreo) {
       this.isMonitoreo = true;
    }
  }
  
  loadCombox() {
    const getEstados = this.maeParametroRepository.getMaestraParametro(
      'CICLO_ESTADO'
    );

    let entidadId = this.profile.entidadId;
    let monitoreo = Utils.obtenerSiEsPantallMonitoreo();
    if(monitoreo) {
      entidadId = JSON.parse(sessionStorage.getItem('entidad')).entidadId;
    }

    const getListCiclos = this.cicloService.getListCiclo(entidadId);

    forkJoin([getEstados, getListCiclos]).subscribe(
      (results) => {

        // console.log("results:",results)
        // return false;
        if (results[1].length < 1) {
          this.toastService.showToast(
            'No se tiene seleccionado ningún ciclo',
            'danger'
          );
        }
        this.estados = results[0];
        this.setCiclo(results[1]);
      },
      (error) => {
        this.toastService.showToast(
          error + ', No se cargo la información correctamente',
          'danger'
        );
      }
    );
  }

  setCiclo(lstCiclo) {
    this.lstCiclos = lstCiclo;
    
    if (this.ciclo.length !== 0 && this.ciclo) {
      this.cicloDefault = this.ciclo.cicloId;
      this.cicloDefaultDesc = this.ciclo.cicloId.anioString;
      if (this.lstCiclos.length > 0) {
        this.anio=0
        let i = 0;
        lstCiclo.forEach((item) => {
          this.lstCiclos[i].fechaIni = this.datePipe.transform(
            item.fechaIni,
            'dd/MM/yyyy'
          );
          this.lstCiclos[i].fechaFin = this.datePipe.transform(
            item.fechaFin,
            'dd/MM/yyyy'
          );
          this.lstCiclos[i].anioString = this.lstCiclos[i].anio.toString();
          this.anio= this.lstCiclos[i].anio
          i += 1;
       
        });
      } else {
        this.cicloDefault = 0;
      }
      const cronograma = this.lstCiclos.find((item) => item.cicloId === this.cicloDefault);
    this.ciclo = cronograma
    sessionStorage.setItem('ciclo', JSON.stringify(this.ciclo));
    if (!(cronograma.estadoCicloId === 1 || cronograma.estadoCicloId === 2)) {
      this.toastService.showToast(
        'El ciclo seleccionado no se encuentra vigente',
        'danger'
      );
    }
    } else {
      this.cicloDefaultDesc = '';
      this.cicloDefault = 0;
      // this.toastService.showToast(
      //   'Para mostrar el cronograma debe registrar un ciclo',
      //   'danger'
      // );
    }
  }

  cambiaCiclo(events) {
    if (this.lstCiclos) {
      const ciclo = this.lstCiclos.find((item) => item.cicloId === events);
      sessionStorage.setItem('ciclo', JSON.stringify(ciclo));
      this.ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
     
    if (!(ciclo.estadoCicloId === 1 || ciclo.estadoCicloId === 2)) {
      this.toastService.showToast(
        'El ciclo seleccionado no se encuentra vigente',
        'danger'
      );
    }
    }
  }

  addCicloSession(events) {
    if (events) {
      const ciclo = events;
      sessionStorage.setItem('ciclo', JSON.stringify(ciclo));
      this.ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
    }
  }

  addCiclo() {
    const add = this.dialog.open(RegistrarCicloComponent, {
      disableClose: true,
    });
    add.afterClosed().subscribe((any) => {
      if (any) {
        this.loadCombox();
        if (this.ciclo.length === 0) {
          this.addCicloSession(any);
        }
      }
    });
  }

  initializeColumns() {
    this.ordersTableColumns = [
      {
        name: 'Año ciclo GDR',
        dataKey: 'anio',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'Fecha inicio',
        dataKey: 'fechaIni',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'Fecha fin',
        dataKey: 'fechaFin',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'Cronograma institucional',
        dataKey: 'descripcion',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'Estado',
        dataKey: 'estadoCicloDesc',
        position: 'left',
        isSortable: true,
      },
    ];
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.lstCiclos);
  }

  editSede(item: Ciclos) {
    this.dataToEdit = item;
  }

  removeSede(item: Ciclos) {
    this.dataToDelete = item;
  }

  getDataExport() {
    return null;
  }
}
