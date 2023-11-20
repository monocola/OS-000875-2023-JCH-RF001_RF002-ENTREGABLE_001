import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { Sort } from '@angular/material/sort';
import { Router } from '@angular/router';
import { EvaluacionConocimientosRepository } from 'src/app/@domain/repository/evaluacion-conocimientos.repository';
import { TableColumn } from 'src/app/@presentation/@common-components/material-table/table-column';
import { ModalConfirmationComponent } from 'src/app/@presentation/@common-components/modal-confirmation/modal-confirmation.component';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { ExportExcelModel } from 'src/app/@presentation/@service/export-excel.service';
import { sortDataTableComponent } from 'src/app/utils/general';

@Component({
  selector: 'serv-talento-examenes',
  templateUrl: './examenes.component.html',
  styleUrls: ['./examenes.component.scss'],
})
export class ExamenesComponent implements OnInit {
  filterForm: FormGroup;
  examenes = [];
  examenesColumns: TableColumn[];
  convocatorias: [];
  perfiles: [];
  searchMode = false;

  constructor(
    private fb: FormBuilder,
    private evaluacionConocimientosService: EvaluacionConocimientosRepository,
    private router: Router,
    private dialog: MatDialog,
    private toastService: ToastService
  ) { }

  ngOnInit(): void {
    this.initializeForm();
    this.initializeColumns();

    this.evaluacionConocimientosService
      .comboConvocatorias()
      .subscribe((res) => {
        this.convocatorias = res;
      });

    this.f.convocatoriaId.valueChanges.subscribe((resConvocatoria) => {
      this.f.perfilId.setValue(null);
      if (resConvocatoria === '') {
        this.f.perfilId.disable();
      } else {
        this.f.perfilId.enable();
        this.evaluacionConocimientosService
          .comboPerfiles(resConvocatoria)
          .subscribe((resPerfil) => {
            this.perfiles = resPerfil;
          });
      }
    });

    this.getLista();
  }

  limpiar() {
    this.f.nombreExamen.setValue(null);
    this.f.convocatoriaId.setValue(null);
    this.f.perfilId.setValue(null);
    this.getLista();
  }

  buscar() {
    this.searchMode = true;
    this.getLista();
  }

  getLista() {
    this.evaluacionConocimientosService
      .listarExamenes(
        null,
        this.f.nombreExamen.value,
        this.f.convocatoriaId.value,
        this.f.perfilId.value
      )
      .subscribe((res) => {
        this.examenes = res;
        this.examenes.map((element) => {
          element.codigoConvocatoria =
            element.codigoConvocatoria.length === 0
              ? '-'
              : element.codigoConvocatoria + ' | ' + element.regimen;
          element.nombrePuesto =
            element.nombrePuesto === 'Pendiente de programación'
              ? '-'
              : element.nombrePuesto;
        });
      });
  }

  get f() {
    return this.filterForm.controls;
  }

  edit(event) {
    this.router.navigateByUrl(
      '/pages/evaluacion-conocimientos/examenes/editar-registro/' +
      event.examenId
    );
  }

  delete(event) {
    const confirmModal = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Eliminar Exámen',
        bodyText: '¿Está seguro de eliminar el examen?',
      },
    });
    confirmModal.afterClosed().subscribe((res) => {
      if (res === true) {
        this.evaluacionConocimientosService
          .deleteExamen(event.examenId)
          .subscribe((mensaje) => {
            this.toastService.showToast(mensaje, 'success');
            this.getLista();
          });
      }
    });
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.examenes);
  }

  initializeForm() {
    this.filterForm = this.fb.group({
      nombreExamen: null,
      convocatoriaId: null,
      perfilId: { value: null, disabled: true },
    });
  }

  initializeColumns() {
    this.examenesColumns = [
      {
        name: '#',
        dataKey: 'examenId',
        position: 'left',
        isSortable: true,
        width: '10%',
      },
      {
        name: 'NOMBRE DEL EXAMEN',
        dataKey: 'nombreExamen',
        position: 'left',
        isSortable: true,
        width: '50%',
      },
      {
        name: 'N° PREGUNTAS',
        dataKey: 'preguntas',
        position: 'left',
        isSortable: true,
        width: '20%',
      },
    ];
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de Exámenes';
    model.headers = ['#', 'NOMBRE DEL EXAMEN', 'N° PREGUNTAS'];
    model.keys = ['examenId', 'nombreExamen', 'preguntas'];
    return model;
  }

  viewExamen($event) {

    sessionStorage.setItem('examen',JSON.stringify($event));
    this.router.navigateByUrl(
      '/pages/evaluacion-conocimientos/examenes/visualizar-examen'
    );
  }
}
