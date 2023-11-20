import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { Sort } from '@angular/material/sort';
import { Organo } from 'src/app/@data/model/organo';
import { UnidadOrganicaRepository } from 'src/app/@domain/repository/unidad-organica.repository';
import { TableColumn } from 'src/app/@presentation/@common-components/material-table/table-column';
import { ModalConfirmationComponent } from 'src/app/@presentation/@common-components/modal-confirmation/modal-confirmation.component';
import { ExportExcelModel } from 'src/app/@presentation/@service/export-excel.service';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { sortDataTableComponent } from 'src/app/utils/general';

@Component({
  selector: 'serv-talento-unidad-organica',
  templateUrl: './unidad-organica.component.html',
})
export class UnidadOrganicaComponent implements OnInit {
  @Input() editDataFromGraph = null;
  @Input() createMode = false;
  @Output() closeOrgano = new EventEmitter();
  @Output() editUnidadOrganoEmmiter = new EventEmitter();

  dataToEdit: Organo = null;
  editMode = false;
  index = 0;

  ordersTableColumns: TableColumn[];
  unidadesOrganicas: any[] = [];

  constructor(
    private unidadOrganicaRepository: UnidadOrganicaRepository,
    private toastService: ToastService,
    private dialog: MatDialog
  ) {}

  ngOnInit(): void {
    this.getUnidadesOrganicasData(false);
    this.initializeColumns();
  }

  handleUpdateEmitter() {
    this.getUnidadesOrganicasData(true);
    this.handleClose(1);
  }

  handleClose(flag) {
    this.closeOrgano.emit(flag);
    if (flag === 1) {
      this.dataToEdit = null;
      this.createMode = false;
    } else {
      this.index = 0;
      this.editMode = false;
    }
  }

  getUnidadesOrganicasData(toUpdateValues: boolean) {
    // this.loading = true;
    this.unidadOrganicaRepository
      .getUnidadesOrganicas(toUpdateValues)
      .subscribe(
        (res) => {
          // this.loading = false;
          this.unidadesOrganicas = res;
          this.unidadesOrganicas.forEach((organo) => {
            if (organo.estado === 'INACTIVO') {
              organo.settings = {};
              organo.settings.disableDelete = true;
              organo.settings.estado = {};
              organo.settings.estado.color = '#EB5757';
            } else {
              organo.settings = {};
              organo.settings.estado = {};
              organo.settings.estado.color = '#0d88bc';
            }
          });
        },
        (err) => {
          // this.loading = false;
          this.toastService.showToast(err, 'danger');
        }
      );
  }

  editOrgano(unidadOrganica: any) {
    window.scrollTo(0, 0);
    this.dataToEdit = unidadOrganica;
    this.createMode = true;
    this.editMode = true;
    this.index = 0;
    this.editUnidadOrganoEmmiter.emit();
  }

  removeOrgano(organo: any) {
    const deleteSede = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Desactivar unidad orgánica',
        bodyText:
          '¿Está seguro de querer deshabilitar la unidad orgánica seleccionada?',
      },
    });
    deleteSede.afterClosed().subscribe((res) => {
      if (res === true) {
        this.unidadOrganicaRepository
          .deleteUnidad(organo.organigramaId)
          .subscribe(
            (unidadDeleted) => {
              this.toastService.showToast(
                'La unidad orgánica se ha deshabilitado correctamente',
                'success'
              );
              this.getUnidadesOrganicasData(true);
            },
            (err) => {
              this.toastService.showToast(
                err || 'Ocurrió un error al desactivar la unidad orgánica',
                'danger'
              );
            }
          );
      }
    });
  }

  initializeColumns() {
    this.ordersTableColumns = [
      {
        name: '#',
        dataKey: 'organigramaId',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'NIVEL',
        dataKey: 'desNivel',
        position: 'left',
        isSortable: true,
      },
      { name: 'SIGLA', dataKey: 'sigla', position: 'left', isSortable: true },
      {
        name: 'UNIDAD ORGÁNICA',
        dataKey: 'unidadOrganica',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'DEPENDENCIA JERÁRQUICA',
        dataKey: 'descripOrganoPadre',
        position: 'left',
        isSortable: true,
      },
      { name: 'ESTADO', dataKey: 'estado', position: 'left', isSortable: true },
    ];
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.unidadesOrganicas);
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de unidades orgánicas';
    model.headers = [
      'ID',
      'NIVEL',
      'SIGLA',
      'UNIDAD ORGÁNICA',
      'DEPENDENCIA JERÁRQUICA',
      'TIPO DOCUMENTO',
      'NUMERO DOCUMENTO',
      'NOMBRES',
      'AP. PATERNO',
      'AP. MATERNO',
      'PAÍS',
      'PUESTO',
      'CELULAR',
      'CORREO LABORAL',
      'ESTADO',
    ];
    model.keys = [
      'organigramaId',
      'desNivel',
      'sigla',
      'unidadOrganica',
      'desNaturaleza',
      'descripOrganoPadre',
      'destipoDocumento',
      'nroDocumento',
      'nombres',
      'apellidoPaterno',
      'apellidoMaterno',
      'puesto',
      'telefono',
      'correo',
      'estado',
    ];
    return model;
  }
}
