import {
  AfterViewInit,
  Component,
  EventEmitter,
  Input,
  OnInit,
  Output,
  ViewChild } from '@angular/core';
import { MatTableDataSource } from '@angular/material/table';
import { MatSort, Sort } from '@angular/material/sort';
import { ExportExcelModel, ExportExcelService } from '../../../../@service/export-excel.service';
import { TableColumn } from '../../../../@common-components/material-table/table-column';
import { MatPaginator } from '@angular/material/paginator';
import { Const } from '../../../../../@data/services/const';
import { from } from 'rxjs';
import { map, toArray } from 'rxjs/operators';

@Component({
  selector: 'serv-talento-tabla-gestion-notas',
  templateUrl: './tabla-gestion-notas.component.html',
  styleUrls: ['./tabla-gestion-notas.component.scss']
})
export class TablaGestionNotasComponent implements OnInit, AfterViewInit {
  @ViewChild(MatPaginator, { static: false }) matPaginator: MatPaginator;
  @ViewChild(MatSort, { static: true }) matSort: MatSort;

  @Input() tableColumns: TableColumn[];
  @Input() title: string = '';

  @Input() isPageable = false;
  @Input() isSortable = false;
  @Input() isFilterable = false;
  @Input() fontSize = 'fs-14';
  @Input() rowHeight = '64px';
  @Input() acciones = false;
  @Input() showTitle = false;
  @Input() pdfTitle: string = '';
  @Input() viewTitle: string = '';
  @Input() exportTitle: string = '';

  @Input() dataExport = new ExportExcelModel();
  @Output() sort: EventEmitter<Sort> = new EventEmitter();

  @Output() revisarAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() showpdfAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() deleteAction: EventEmitter<any> = new EventEmitter<any>();

  paginationSizes: number[] = [5, 10, 15];
  defaultPageSize = this.paginationSizes[1];

  tableDataSource = new MatTableDataSource([]);
  displayedColumns: string[];
  const = Const;

  @Input() set tableData(data: any[]) {
    this.setTableDataSource(data);
  }

  constructor(
    private exportExcelService: ExportExcelService,
  ) {}

  ngOnInit(): void {
    const columnNames = this.tableColumns.map(
      (tableColumn: TableColumn) => tableColumn.name
    );
    this.displayedColumns = [
      ...columnNames,
      'Acciones',
    ];
  }

  ngAfterViewInit(): void {
    this.tableDataSource.paginator = this.matPaginator;
    this.tableDataSource.sort = this.matSort;
  }

  exportData() {
    const datos = this.tableDataSource.filteredData;
    this.dataExport.title = this.exportTitle;
    from(datos)
      .pipe(
        map((value) => {
          let datanew = {};
          for (const key of this.dataExport.keys) {
            datanew[key] = value[key];
          }
          return datanew;
        })
      )
      .pipe(toArray())
      .subscribe((value) => {
        this.dataExport.data = value;
        this.exportExcelService.exportExcel(this.dataExport);
      });
  }


  setTableDataSource(data: any) {
    this.tableDataSource = new MatTableDataSource<any>(data);
    this.tableDataSource.paginator = this.matPaginator;
    this.tableDataSource.sort = this.matSort;
  }

  applyFilter(event: Event) {
    const filterValue = (event.target as HTMLInputElement).value;
    this.tableDataSource.filter = filterValue.trim().toLowerCase();
  }

  sortTable(sortParameters: Sort) {
    if (sortParameters.active !== 'codigoEtapa') {
      sortParameters.active = this.tableColumns.find(
        (column) => column.name === sortParameters.active
      ).dataKey;
      this.sort.emit(sortParameters);
    } else {
      this.sort.emit(sortParameters);
    }
  }

  showRevisar(item: any) {
    return item.desEstado != null && item.nota != null  && item.tipoEvaluacion === 1;
  }

  showPDF(item: any) {
    return item.desEstado != null && item.nota != null && item.tipoEvaluacion === 1;
  }



}
