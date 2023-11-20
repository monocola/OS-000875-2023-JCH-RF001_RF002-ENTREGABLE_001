import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { Sort } from '@angular/material/sort';
import { CabeceraMaestra } from 'src/app/@data/model/cabeceraMaestra';
import { CabeceraMaestraDetail } from 'src/app/@data/model/cabeceraMaestraDetail';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';
import { ParameterRepository } from 'src/app/@domain/repository/parameter.repository';
import { TableColumn } from 'src/app/@presentation/@common-components/material-table/table-column';
import { ModalConfirmationComponent } from 'src/app/@presentation/@common-components/modal-confirmation/modal-confirmation.component';
import { ExportExcelModel } from 'src/app/@presentation/@service/export-excel.service';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import {
  setValidOrInvalidColors,
  sortDataTableComponent,
} from 'src/app/utils/general';
import { forkJoin } from 'rxjs';
import { ModalCreacionDetalleMaestraComponent } from './modal-creacion-detalle-maestra/modal-creacion-detalle-maestra.component';

@Component({
  selector: 'serv-talento-maestra-servir',
  templateUrl: './maestra-servir.component.html',
  styleUrls: ['./maestra-servir.component.scss'],
})
export class TablaMaestraComponent implements OnInit {
  filterForm: FormGroup;
  searchMode = false;
  listSiglas = false;

  estados = [];
  comboTablaMaestra: CabeceraMaestra[] = [];
  dataTablaMaestra: CabeceraMaestraDetail[] = [];
  dataTablaMaestraAutocomplete: CabeceraMaestraDetail[] = [];
  siglas: any[] = [];

  maestraTableColumns: TableColumn[];
  dataToEdit: CabeceraMaestraDetail = null;

  constructor(
    private fb: FormBuilder,
    private maestraService: MaestraRepository,
    private parametrosRepository: ParameterRepository,
    private toastService: ToastService,
    private dialog: MatDialog
  ) {}

  ngOnInit(): void {
    this.initializeForm();
    this.loadCombox();
    this.initializeColumns();
  }

  get f() {
    return this.filterForm.controls;
  }

  initializeForm() {
    this.filterForm = this.fb.group({
      tablaMaestra: '',
      nombreCamposMaestra: '',
      sigla: '',
      estado: '',
    });
  }

  openModalRegister(createMode: boolean = true) {
    const registerDialog = this.dialog.open(
      ModalCreacionDetalleMaestraComponent,
      {
        data: {
          tablaMaestraSelected: this.filterForm.get('tablaMaestra').value,
          createMode,
          estados: this.estados,
          tablaMaestra: this.comboTablaMaestra,
          dataToEdit: this.dataToEdit,
        },
      }
    );
    registerDialog.afterClosed().subscribe((res) => {
      if (res) {
        if (!this.searchMode) {
          this.clearForm();
          this.filterForm.patchValue({
            tablaMaestra: res.tablaMaestra,
          });
          this.searchMode = true;
          this.setDetailTable();
        } else {
          this.setDetailTable();
          this.filterForm.patchValue({
            nombreCamposMaestra: '',
            sigla: '',
            estado: '',
          });
        }
      }
      this.dataToEdit = null;
    });
  }

  loadCombox() {
    const getEstado = this.parametrosRepository.getEstadoRegistro();
    const getCabeceras = this.maestraService.getMaestraList();
    forkJoin([getEstado, getCabeceras]).subscribe((results) => {
      this.estados = results[0];
      this.comboTablaMaestra = results[1];
    });
  }

  clearForm() {
    this.initializeForm();
    this.searchMode = false;
    this.dataTablaMaestra = [];
  }

  search() {
    if (!this.searchMode) {
      this.searchMode = true;
      this.setDetailTable();
    } else {
      this.maestraService
        .filtrarMaestraDetalle(this.filterForm.value)
        .subscribe(
          (res) => {
            this.dataTablaMaestra = res.slice(0);
            this.dataTablaMaestra = setValidOrInvalidColors(
              this.dataTablaMaestra,
              'estadoRegistro'
            );
            this.dataTablaMaestraAutocomplete = this.dataTablaMaestra.slice(0);
            if (this.listSiglas) {
              this.siglas = this.dataTablaMaestra.map((item) => item.sigla);
              this.listSiglas = true;
            }
          },
          (err) => {
            this.toastService.showToast(err, 'danger');
          }
        );
    }
  }

  changeMaestra() {
    if (this.searchMode) {
      if (this.f.tablaMaestra.value) {
        this.filterForm.patchValue({
          nombreCamposMaestra: '',
          sigla: '',
          estado: '',
        });
        this.setDetailTable();
      } else {
        this.clearForm();
      }
    }
  }

  clearAutocomplete() {
    this.dataTablaMaestraAutocomplete = [];
    this.searchMode = false;
    this.listSiglas = false;
    this.f.nombreCamposMaestra.patchValue('');
    this.f.estado.patchValue('');
    this.f.sigla.patchValue('');
  }

  setDetailTable() {
    this.maestraService
      .getMaestraListDetail(this.f.tablaMaestra.value)
      .subscribe(
        (res) => {
          this.dataTablaMaestra = res;
          this.dataTablaMaestraAutocomplete = res;
          this.siglas = this.dataTablaMaestra.map((item) => item.sigla);
          this.dataTablaMaestra = setValidOrInvalidColors(
            this.dataTablaMaestra,
            'estadoRegistro'
          );
        },
        (err) => {
          this.toastService.showToast(err, 'danger');
        }
      );
  }

  // ---------------------------------- TABLA ---------------------------------- //
  // @SONAR_DECLARE_COLUM_TABLE_START@
  initializeColumns() {
    this.maestraTableColumns = [
      {
        name: '#',
        dataKey: 'maeDetalleId',
        position: 'left',
        isSortable: true,
        width: '5%',
      },
      {
        name: 'NOMBRE TABLA MAESTRA',
        dataKey: 'descripcionMaestra',
        position: 'left',
        isSortable: true,
        width: '15%',
      },
      {
        name: 'NOMBRE COMPLETO',
        dataKey: 'descripcion',
        position: 'left',
        isSortable: true,
        width: '35%',
      },
      {
        name: 'NOMBRE CORTO',
        dataKey: 'descripcionCorta',
        position: 'left',
        isSortable: true,
        width: '15%',
      },
      {
        name: 'SIGLA',
        dataKey: 'sigla',
        position: 'left',
        isSortable: true,
        width: '10%',
      },
      {
        name: 'ESTADO',
        dataKey: 'estado',
        position: 'left',
        isSortable: true,
        width: '10%',
      },
    ];
  }
  // @SONAR_DECLARE_COLUM_TABLE_END@
  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.dataTablaMaestra);
  }

  editMaestra(detalleMaestra: CabeceraMaestraDetail) {
    this.dataToEdit = detalleMaestra;
    this.openModalRegister(false);
  }

  removeMaestra(e: CabeceraMaestraDetail) {
    const deleteMaestra = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Desactivar detalle de maestra',
        bodyText:
          '¿Está seguro de querer deshabilitar la maestra seleccionada?',
      },
    });

    deleteMaestra.afterClosed().subscribe((res) => {
      if (res === true) {
        this.maestraService.deleteMaestraDetalle(e.maeDetalleId).subscribe(
          (maestraDeleted) => {
            this.toastService.showToast(
              'El detalle ha sido borrado exitosamente',
              'success'
            );
            this.setDetailTable();
          },
          (err) => this.toastService.showToast(err, 'danger')
        );
      }
    });
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de sedes';
    model.headers = [
      'ID',
      'NOMBRE DE TABLA MAESTRA',
      'NOMBRE COMPLETO',
      'NOMBRE CORTO',
      'SIGLA',
      'DESCRIPCION',
      'ESTADO',
    ];
    model.keys = [
      'maeDetalleId',
      'descripcionMaestra',
      'descripcion',
      'descripcionCorta',
      'sigla',
      'referencia',
      'estado',
    ];
    return model;
  }
}
