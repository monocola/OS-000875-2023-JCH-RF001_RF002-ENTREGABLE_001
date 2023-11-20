import { forkJoin } from 'rxjs';
import { FormBuilder, FormGroup } from '@angular/forms';
import { ParameterRepository } from 'src/app/@domain/repository/parameter.repository';
import { Component, OnInit } from '@angular/core';
import { TableColumn } from 'src/app/@presentation/@common-components/material-table/table-column';
import { sortDataTableComponent } from 'src/app/utils/general';
import { Sort } from '@angular/material/sort';
import { ExportExcelModel } from 'src/app/@presentation/@service/export-excel.service';
import { MatDialog } from '@angular/material/dialog';
import { ModalConfirmationComponent } from 'src/app/@presentation/@common-components/modal-confirmation/modal-confirmation.component';
import { SedesRepository } from 'src/app/@domain/repository/sede.repository';
import { Sede } from 'src/app/@data/model/sede';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { ModalCreacionSedesComponent } from './modal-creacion-sedes/modal-creacion-sedes.component';

@Component({
  selector: 'serv-talento-sedes',
  templateUrl: './sedes.component.html',
  styleUrls: ['./sedes.component.scss'],
})
export class SedesComponent implements OnInit {
  sedes: Sede[] = [];
  estados = [];

  dataToEdit: Sede = null;

  sedesTableColumns: TableColumn[];

  filterForm: FormGroup = null;
  searchMode = false;

  constructor(
    private fb: FormBuilder,
    private parametrosRepository: ParameterRepository,
    private sedesRepository: SedesRepository,
    private dialog: MatDialog,
    private toastService: ToastService
  ) {
    this.initializeForm();
  }

  ngOnInit(): void {
    this.loadCombox();
    this.initializeColumns();
  }

  initializeForm() {
    this.filterForm = this.fb.group({
      sede: '',
      departamento: '',
      provincia: '',
      distrito: '',
      estado: '',
    });
  }

  clearForm() {
    this.initializeForm();
    this.search();
  }

  get f() {
    return this.filterForm.controls;
  }

  loadCombox() {
    this.searchMode = false;
    const getEstados = this.parametrosRepository.getEstadoRegistro();
    const getSedes = this.sedesRepository.getSedesByFiltro(null);
    forkJoin([getEstados, getSedes]).subscribe((results) => {
      this.estados = results[0];
      this.sedes = results[1];
      this.formatColor();
    });
  }

  search() {
    this.searchMode = true;
    const body = this.filterForm.getRawValue();
    this.sedesRepository.getSedesByFiltro(body).subscribe(
      (res) => {
        this.sedes = res;
        this.formatColor();
        if (this.sedes.length === 0) {
          this.toastService.showToast(
            'No se encontraron resultados',
            'primary'
          );
        }
      },
      (err) => {
        this.toastService.showToast(err.message, 'danger');
      }
    );
  }

  formatColor() {
    this.sedes.forEach((sede) => {
      if (sede.estadoId === '0') {
        sede.settings = {};
        sede.settings.disableDelete = true;
        sede.settings.estado = {};
        sede.settings.estado.color = '#eb5757';
      } else {
        sede.settings = {};
        sede.settings.estado = {};
        sede.settings.estado.color = '#0d88bc';
      }
    });
  }

  openModalRegister(createMode: boolean = true) {
    const registerDialog = this.dialog.open(ModalCreacionSedesComponent, {
      data: {
        createMode,
        estados: this.estados,
        dataToEdit: this.dataToEdit,
      },
    });

    registerDialog.afterClosed().subscribe((res) => {
      this.dataToEdit = null;
      if (res) {
        // Se registró una sede
        this.initializeForm();
        this.search();
      }
    });
  }

  // ---------------------------------- TABLA ---------------------------------- //

  editSede(item: Sede) {
    this.dataToEdit = item;
    this.openModalRegister(false);
  }

  removeSede(item: Sede) {
    const deleteSede = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Desactivar sede',
        bodyText: '¿Está seguro de querer deshabilitar la sede seleccionada?',
      },
    });
    deleteSede.afterClosed().subscribe((res) => {
      if (res === true) {
        this.sedesRepository.deleteSede(item.sedeId).subscribe(
          (sedeDeleted) => {
            this.toastService.showToast(
              'La sede ha sido desactivada correctamente',
              'success'
            );
            this.initializeForm();
            this.search();
          },
          (err) => {
            this.toastService.showToast(err.message, 'danger');
          }
        );
      }
    });
  }

  initializeColumns() {
    this.sedesTableColumns = [
      { name: '#', dataKey: 'sedeId', position: 'left', isSortable: true },
      {
        name: 'NOMBRE DE SEDE',
        dataKey: 'nombreSede',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'DIRECCIÓN',
        dataKey: 'direccion',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'DEPARTAMENTO',
        dataKey: 'departamento',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'PROVINCIA',
        dataKey: 'provincia',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'DISTRITO',
        dataKey: 'distrito',
        position: 'left',
        isSortable: true,
      },
      { name: 'ESTADO', dataKey: 'estado', position: 'left', isSortable: true },
    ];
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.sedes);
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de sedes';
    model.headers = [
      'ID',
      'NOMBRE DE SEDE',
      'DIRECCIÓN',
      'DEPARTAMENTO',
      'PROVINCIA',
      'DISTRITO',
      'ESTADO',
    ];
    model.keys = [
      'sedeId',
      'nombreSede',
      'direccion',
      'departamento',
      'provincia',
      'distrito',
      'estado',
    ];
    return model;
  }
}
