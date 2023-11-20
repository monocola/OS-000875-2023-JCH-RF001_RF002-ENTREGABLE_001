import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { sortDataTableComponent } from 'src/app/utils/general';
import { Sort } from '@angular/material/sort';
import { TableColumn } from '../../../@common-components/material-table/table-column';
import { EvaluacionConocimientosRepository } from 'src/app/@domain/repository/evaluacion-conocimientos.repository';
import { ModalConfirmationComponent } from '../../../@common-components/modal-confirmation/modal-confirmation.component';
import { ExportExcelModel } from '../../../@service/export-excel.service';
import { MatDialog } from '@angular/material/dialog';
import { EvaluacionConService } from '../evaluacion-conocimientos.service';
import { Router } from '@angular/router';

@Component({
  selector: 'serv-talento-categoria',
  templateUrl: './categoria.component.html',
  styleUrls: ['./categoria.component.scss']
})

export class CategoriaComponent implements OnInit {

  filterForm: FormGroup;
  listaCategoriaColumns = [];
  categorias: any[] = [];
  categoriasColumns: TableColumn[];

  constructor(
    private fb: FormBuilder,
    private evaluacionConocimientosService: EvaluacionConocimientosRepository,
    private dialog: MatDialog,
    public helperService: EvaluacionConService,
    private router: Router
  ) { }

  ngOnInit(): void {
    this.initializeForm();
    this.getBandejaCategoria();
    this.helperService.initializeValues();
  }

  getBandejaCategoria() {
    this.evaluacionConocimientosService
      .buscarBandejaCategoria(null, null)
      .subscribe((res) => {
        this.categorias = [...res];
      });
  }
  getBuscarCategoria() {
    this.evaluacionConocimientosService
      .buscarBandejaCategoria(null, this.filterForm.controls.txtCategoria.value)
      .subscribe((res) => {
        this.categorias = [...res];
      });
  }

  get f() {
    return this.filterForm.controls;
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.categorias);
  }

  clear() {
    this.initializeForm();
    this.getBandejaCategoria();
  }

  initializeForm() {
    this.filterForm = this.fb.group({
      txtCategoria: '',
    });

    this.categoriasColumns = [
      {
        name: '#',
        dataKey: 'categoriaId',
        position: 'left',
        isSortable: true,
        width: '10%',
      },

      {
        name: 'CATEGORIA',
        dataKey: 'descripcion',
        position: 'left',
        isSortable: true,
        width: '40%',
      },
      {
        name: 'N° PREGUNTAS',
        dataKey: 'cantidadPreguntas',
        position: 'center',
        isSortable: true,
        width: '20%',
      },
      {
        name: 'FECHA DE MODIFICACIÓN',
        dataKey: 'fechaModificacion',
        position: 'center',
        isSortable: true,
        width: '20%',
      },
    ];
  }

  configCategoria(e) {
    this.helperService.initializeValues();
    this.helperService.enviarCategoria(e);
    this.router.navigateByUrl('pages/evaluacion-conocimientos/categorias/addcategorias');
  }

  removeCategoria(categoria) {
    const confirmModal = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Eliminar categoria',
        bodyText: '¿Está seguro de continuar?',
      },
    });
    confirmModal.afterClosed().subscribe((res) => {
      if (res === true) {
        this.evaluacionConocimientosService.deleteCategoria(categoria.categoriaId).subscribe(() => {
          this.getBandejaCategoria();
        });
      }
    });
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de categorias';
    model.headers = [
      '#',
      'CATEGORIA',
      'N° PREGUNTAS',
      'FECHA DE MODIFICACION',
      'ESTADO',
    ];
    model.keys = [
      'categoriaId',
      'descripcion',
      'cantidadPreguntas',
      'fechaModificacion',
      'estadoRegistro',
    ];
    return model;
  }
}


