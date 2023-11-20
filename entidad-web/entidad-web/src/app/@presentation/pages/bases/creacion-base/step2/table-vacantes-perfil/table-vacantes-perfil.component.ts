import {
  AfterViewInit,
  Component,
  EventEmitter,
  Input,
  OnInit,
  Output,
  ViewChild,
} from '@angular/core';
import { MatPaginator } from '@angular/material/paginator';
import { MatSort, Sort } from '@angular/material/sort';
import { MatTableDataSource } from '@angular/material/table';
import { from } from 'rxjs';
import { map, toArray } from 'rxjs/operators';
import { Const } from 'src/app/@data/services/const';
import { TableColumn } from 'src/app/@presentation/@common-components/material-table/table-column';
import {
  ExportExcelModel,
  ExportExcelService,
} from 'src/app/@presentation/@service/export-excel.service';

@Component({
  selector: 'serv-talento-table-vacantes-perfil',
  templateUrl: './table-vacantes-perfil.component.html',
  styleUrls: ['./table-vacantes-perfil.component.scss'],
})
export class TableVacantesPerfilComponent implements OnInit, AfterViewInit {
  @ViewChild(MatPaginator, { static: false }) matPaginator: MatPaginator;
  @ViewChild(MatSort, { static: true }) matSort: MatSort;

  @Input() tableColumns: TableColumn[];
  @Input() title: string = '';
  @Input() dataExport = new ExportExcelModel();
  @Input() showDelete = true;
  @Input() showCopy = true;

  @Output() deleteEmmiter: EventEmitter<any> = new EventEmitter<any>();
  @Output() editEmmiter: EventEmitter<any> = new EventEmitter<any>();
  @Output() copyEmmiter: EventEmitter<any> = new EventEmitter<any>();
  @Output() perfilEmitter: EventEmitter<any> = new EventEmitter<any>();

  @Output() sort: EventEmitter<Sort> = new EventEmitter();

  acciones = true;
  paginationSizes: number[] = [5, 10, 15];
  defaultPageSize = this.paginationSizes[1];

  tableDataSource = new MatTableDataSource([]);
  displayedColumns: string[];
  fontSize = 'fs-13';

  const = Const;

  @Input() set tableData(data: any[]) {
    this.setTableDataSource(data);
  }

  constructor(private exportExcelService: ExportExcelService) {}

  ngOnInit(): void {
    const columnNames = this.tableColumns.map(
      (tableColumn: TableColumn) => tableColumn.name
    );
    this.displayedColumns = ['Perfil', ...columnNames, 'Acciones'];
  }

  ngAfterViewInit(): void {
    this.tableDataSource.paginator = this.matPaginator;
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
    this.tableDataSource.paginator = this.matPaginator;
    this.tableDataSource.sort = this.matSort;
  }

  applyFilter(event: Event) {
    const filterValue = (event.target as HTMLInputElement).value;
    this.tableDataSource.filter = filterValue.trim().toLowerCase();
  }

  sortTable(sortParameters: Sort) {
    switch (sortParameters.active) {
      case 'estado':
        this.sort.emit(sortParameters);
        break;
      case 'perfil':
        this.sort.emit(sortParameters);
        break;
      default:
        sortParameters.active = this.tableColumns.find(
          (column) => column.name === sortParameters.active
        ).dataKey;
        this.sort.emit(sortParameters);
        break;
    }
  }
}
