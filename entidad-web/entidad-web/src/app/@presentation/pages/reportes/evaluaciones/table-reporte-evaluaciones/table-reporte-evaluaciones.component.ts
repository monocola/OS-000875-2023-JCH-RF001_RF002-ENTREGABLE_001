import { AfterViewInit, Component, EventEmitter, Input, OnInit, Output, ViewChild } from '@angular/core';
import { MatSort, Sort } from '@angular/material/sort';
import { MatTableDataSource } from '@angular/material/table';
import { Const } from '../../../../../@data/services/const';
import { TableColumn } from '../../../../@common-components/material-table/table-column';
import { ExportExcelModel, ExportExcelService } from '../../../../@service/export-excel.service';
import { from } from 'rxjs';
import { map, toArray } from 'rxjs/operators';

@Component({
  selector: 'serv-talento-table-reporte-evaluaciones',
  templateUrl: './table-reporte-evaluaciones.component.html',
  styleUrls: ['./table-reporte-evaluaciones.component.scss']
})
export class TableReporteEvaluacionesComponent implements OnInit, AfterViewInit {

  @ViewChild(MatSort, { static: true }) matSort: MatSort;

  tableDataSource = new MatTableDataSource([]);
  displayedColumns: string[];
  const = Const;

  @Input() title: string = '';
  @Input() tamanio: number = 0;
  @Input() tableColumns: TableColumn[];
  @Input() pageIndex: number = 0;
  @Input() paginationSizes: number[] = [5, 10, 15];
  @Input() dataExport = new ExportExcelModel();
  @Input() defaultPageSize = this.paginationSizes[0];

  @Output() sort: EventEmitter<Sort> = new EventEmitter();
  @Output() openDetalleEvalPostulante = new EventEmitter();
  @Output() pageEvent: EventEmitter<any> = new EventEmitter<any>();

  @Input() set tableData(data: any[]) {
    this.setTableDataSource(data);
  }

  constructor(private exportExcelService: ExportExcelService) {}

  ngOnInit(): void {
    const columnNames = this.tableColumns.map(
      (tableColumn: TableColumn) => tableColumn.name
    );
    this.displayedColumns = [...columnNames,  'eval', 'codigo', 'regimen', 'nombresapellidos', 'rnssc', 'redereci', 'redam', 'nota', 'resultado', 'Estado'];
  }

  ngAfterViewInit(): void {
    this.tableDataSource.sort = this.matSort;
  }

  exportData() {
    const datos = this.tableDataSource.filteredData;
    this.dataExport.title = this.title;
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
    this.tableDataSource.sort = this.matSort;
  }

  applyFilter(event: Event) {
    const filterValue = (event.target as HTMLInputElement).value;
    this.tableDataSource.filter = filterValue.trim().toLowerCase();
  }

  sortTable(sortParameters: Sort) {
    let columns: boolean = true;
    if (sortParameters.active === 'estado') {
      columns = false;
    }
    if (columns) {
      sortParameters.active = this.tableColumns.find(
        (column) => column.name === sortParameters.active
      ).dataKey;
      this.sort.emit(sortParameters);
    } else {
      this.sort.emit(sortParameters);
    }
  }

  openDetallePostulante(row) {
    this.openDetalleEvalPostulante.emit(row);
  }


}
