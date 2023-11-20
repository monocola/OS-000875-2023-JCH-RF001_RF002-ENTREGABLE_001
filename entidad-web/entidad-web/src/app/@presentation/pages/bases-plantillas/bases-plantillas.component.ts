import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { Sort } from '@angular/material/sort';
import { Router } from '@angular/router';
import { forkJoin } from 'rxjs';
import { Const } from 'src/app/@data/services/const';
import { BasesPlantillasRepository } from 'src/app/@domain/repository/bases-plantillas.repository';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';
import { ParameterRepository } from 'src/app/@domain/repository/parameter.repository';
import { sortDataTableComponent } from 'src/app/utils/general';
import { TableColumn } from '../../@common-components/material-table/table-column';
import { ModalConfirmationComponent } from '../../@common-components/modal-confirmation/modal-confirmation.component';
import { ExportExcelModel } from '../../@service/export-excel.service';
import { ToastService } from '../../@common-components/toast';
import { CreacionFormBaseService } from './creacion-form-base/creacion-form-base.service';

@Component({
  selector: 'serv-talento-bases-plantillas',
  templateUrl: './bases-plantillas.component.html',
})
export class BasesPlantillasComponent implements OnInit {
  filterForm: FormGroup;
  tiposInforme = [];
  estados = [];

  plantillasBase = [];
  searchMode = false;

  plantillasColumns: TableColumn[];
  rol: any = JSON.parse(sessionStorage.getItem('roles'));

  constructor(
    private fb: FormBuilder,
    private maestraService: MaestraRepository,
    private parametrosRepository: ParameterRepository,
    private plantillasBaseService: BasesPlantillasRepository,
    private helperService: CreacionFormBaseService,
    private router: Router,
    private dialog: MatDialog,
    private toastService: ToastService
  ) {}

  ngOnInit(): void {
    this.initializeForm();
    this.initializeColumns();
    this.loadCombox();
    this.getPlantillas();
  }

  initializeForm() {
    this.filterForm = this.fb.group({
      tipoInforme: '',
      nombreInforme: '',
      estado: '',
    });
  }

  clear() {
    this.searchMode = false;
    this.initializeForm();
    this.getPlantillas();
  }

  loadCombox() {
    const getTiposInformes = this.maestraService.getMaestraDetalleByCod(
      'TIP_INF'
    );
    const getEstados = this.parametrosRepository.getEstadoRegistro();
    forkJoin([getTiposInformes, getEstados]).subscribe((results) => {
      this.tiposInforme = Const.R_ADMIN_ENTIDAD !== this.rol.rolId ? results[0] : results[0].filter(it => it.codProg !== "2");// results[0];
      this.estados = results[1];
    });
  }

  getPlantillas() {
    const tramaBody = this.filterForm.getRawValue();
    this.plantillasBaseService.getPlantillasBase(tramaBody).subscribe((res) => {
      this.searchMode = true;
      this.plantillasBase = res;
    });
  }

  // De la tabla

  editPlantilla(plantilla) {
    this.helperService.dataToEdit = plantilla;
    let codPro = this.helperService.dataToEdit.codProg;
    this.helperService.tipoInforme = this.tiposInforme.find(
      (ti) => ti.maeDetalleId === plantilla.tipoInfo
    );
    sessionStorage.setItem(
      'tipoInforme',
      JSON.stringify(this.helperService.tipoInforme)
    );
    // if (this.helperService.tipoInforme.codProg === Const.INF_BONIFICACION) {
    //  this.router.navigateByUrl('pages/gestionplantillas/bonificaciones');
    if (codPro === Const.INF_BONIFICACION) {
      this.toastService.showToast( 'La edición de Bonificación para administrador Entidad no esta disponible','warning');
    } else {
      this.router.navigateByUrl('pages/gestionplantillas/base-legal');
    }
  }

  removePlantilla(event) {
    const idDetalle = event.informeDetalleId;
    const modalConfirmacion = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Desactivar plantilla',
        bodyText:
          '¿Está seguro de querer deshabilitar la plantilla seleccionada?',
      },
    });
    modalConfirmacion.afterClosed().subscribe((res) => {
      if (res) {
        this.plantillasBaseService
          .deleteBonificacionList(idDetalle)
          .subscribe(() => {
            this.plantillasBase = this.plantillasBase.filter(
              (p) => p.informeDetalleId !== event.informeDetalleId
            );
            this.toastService.showToast(
              'La plantilla ha sido desactivada con éxito',
              'success'
            );
          });
      }
    });
  }

  initializeColumns() {
    this.plantillasColumns = [
      {
        name: '#',
        dataKey: 'informeDetalleId',
        position: 'left',
        isSortable: true,
        width: '10%',
      },
      {
        name: 'Tipo de informe',
        dataKey: 'nombretipoInfo',
        position: 'left',
        isSortable: true,
        width: '30%',
      },
      {
        name: 'Nombre',
        dataKey: 'titulo',
        position: 'left',
        isSortable: true,
        width: '40%',
      },
      {
        name: 'Estado',
        dataKey: 'estado',
        position: 'left',
        isSortable: true,
        width: '10%',
      },
    ];
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.plantillasBase);
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de plantillas de las bases';
    model.headers = ['ID', 'TIPO DE INFORME', 'NOMBRE', 'ESTADO'];
    model.keys = ['informeDetalleId', 'nombretipoInfo', 'titulo', 'estado'];
    return model;
  }

  get f() {
    return this.filterForm.controls;
  }
}
