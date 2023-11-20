import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { MaestraParametro } from '../../../../@data/model/maestra-parametro';
import { MaestraParametroRepository } from '../../../../@domain/repository/maestra-parametro.repository';
import { CicloService } from '../../../../@data/services/ciclo.service';
import { AuthenticationRepository } from '../../../../@domain/repository/authentication.repository';
import { ToastService } from '../../../@common-components/toast';
import { MatDialog } from '@angular/material/dialog';
import { TableColumn } from '../../../@common-components/material-table/table-column';
import { forkJoin } from 'rxjs';
import {
  Actividades,
  HistorialActividad,
} from '../../../../@data/model/actividades';
import { CronogramaRepository } from '../../../../@domain/repository/cronograma.repository';
import moment from 'moment';

@Component({
  selector: 'serv-talento-historial-cronograma',
  templateUrl: './historial-cronograma.component.html',
  styleUrls: ['./historial-cronograma.component.scss']
})
export class HistorialCronogramaComponent implements OnInit {
  searchMode = false;

  constructor(
    private fb: FormBuilder,
    private maeParametroRepository: MaestraParametroRepository,
    private cicloService: CicloService,
    private authenticationService: AuthenticationRepository,
    private toastService: ToastService,
    private dialog: MatDialog,
    private cronogramaRepository: CronogramaRepository
  ) {}

  cicloDefaultDesc;
  cicloDefault;
  frm: FormGroup = null;
  etapa: MaestraParametro[];
  resolucion: MaestraParametro[];
  lstActividad: MaestraParametro[];
  profile = this.authenticationService.getCurrentUserValue;
  holderText = 'Buscar por Etapa, actividades, fecha de inicio, fecha fin, responsable';
  lstHistActividades: HistorialActividad[] = [];
  ordersTableColumns: TableColumn[];
  ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
  lstResolucion: any[] = [];

  ngOnInit(): void {
    this.loadCombox();
    this.initForm();
    this.initializeColumns();
    this.search();
    this.getResoluciones();
    this.getActividades();
  }

  loadCombox() {
    const getEtapa = this.maeParametroRepository.getMaestraParametro(
      'ETAPA_GDR'
    );
    forkJoin([getEtapa]).subscribe(
      (results) => {
        this.etapa = results[0];
        this.setCiclo();
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

  initForm() {
    this.frm = this.fb.group({
      etapa: '',
      resolucion: '',
      actividad: '',
    });
  }

  initializeColumns() {
    this.ordersTableColumns = [
      {
        name: 'N°',
        dataKey: 'numeracion',
        position: 'left',
        isSortable: true,
        settings: '',
      },
      {
        name: 'Etapas',
        dataKey: 'descripcionEtapa',
        position: 'left',
        isSortable: true,
        settings: 'flagEtapa',
      },
      {
        name: 'Actividades',
        dataKey: 'descripcionActividad',
        position: 'left',
        isSortable: true,
        settings: 'flagActividad',
      },
      {
        name: 'Fecha de inicio',
        dataKey: 'fechaInicio',
        position: 'left',
        isSortable: true,
        settings: 'flagFechaFin',
      },
      {
        name: 'Fecha fin',
        dataKey: 'fechaFin',
        position: 'left',
        isSortable: true,
        settings: 'flagFechaInicio',
      },
      {
        name: 'Responsable',
        dataKey: 'descripcionResponsable',
        position: 'left',
        isSortable: true,
        settings: 'flagDescripcionResponsable',
      },
    ];
  }

  setCiclo() {
    if (this.ciclo.length !== 0 && this.ciclo) {
      this.cicloDefaultDesc = ' - ' + this.ciclo.anio;
      this.cicloDefault = this.ciclo.cicloId;
    } else {
      this.cicloDefaultDesc = '';
    }
  }

  clear() {
    this.loadCombox();
    this.initForm();
    this.initializeColumns();
    this.search();
    this.getResoluciones();
    this.getActividades();
  }

  search() {
    this.searchMode = true;
    const body = this.frm.getRawValue();
    this.cronogramaRepository
      .getHistorialModificacionAct(body, this.ciclo.cronogramaId)
      .subscribe(
        (res: any []) => {
          console.log (res);
          this.lstHistActividades = res;
          this.lstHistActividades.forEach ((item: any) => {
            if (item.fechaInicio) {
              item.fechaInicio = moment (item.fechaInicio).format ('DD[/]MM[/]YYYY');
            }

            if (item.fechaFin) {
              item.fechaFin = moment (item.fechaFin).format ('DD[/]MM[/]YYYY');
            }
          });

          if (this.lstHistActividades.length === 0) {
            this.toastService.showToast(
              'No se encontraron resultados',
              'primary'
            );
          } else {
            for (let i = 0; i < this.lstHistActividades.length; i++) {
              this.lstHistActividades[i].settings = 0;
            }
          }
        },
        (err) => {
          this.toastService.showToast(err.message, 'danger');
        }
      );
  }

  getResoluciones() {
    this.cronogramaRepository
      .getResoluciones(this.ciclo)
      .subscribe((item: any) => {
        if (item.status.success) {
          this.resolucion = item.payload;
        }
      });
  }

  getActividades() {
    this.frm.get('actividad').setValue('');
    this.cronogramaRepository
      .getCboActividades(this.ciclo.cronogramaId)
      .subscribe((item: any) => {
        console.log("si entro")
        if (item.status.success) {
          this.lstActividad = item.payload;
        }
      });
  }

  sortData(events) {}
  editAct(events) {}
  delAct(events) {}
  getDataExport() {}
}
