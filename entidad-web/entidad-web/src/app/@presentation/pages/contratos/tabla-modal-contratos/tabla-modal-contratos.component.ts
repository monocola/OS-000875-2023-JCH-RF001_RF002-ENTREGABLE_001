import { AfterViewInit, Component, EventEmitter, Input, OnInit, Output, ViewChild } from '@angular/core';
import { MatPaginator } from '@angular/material/paginator';
import { MatSort, Sort } from '@angular/material/sort';
import { MatTableDataSource } from '@angular/material/table';
import { TableColumn } from 'src/app/@presentation/@common-components/material-table/table-column';
import { Const } from 'src/app/@data/services/const';

@Component({
  selector: 'serv-talento-tabla-modal-contratos',
  templateUrl: './tabla-modal-contratos.component.html',
  styleUrls: ['./tabla-modal-contratos.component.scss'],
})
export class TablaModalContratosComponent implements OnInit, AfterViewInit {
  @ViewChild(MatPaginator, { static: false }) matPaginator: MatPaginator;
  @ViewChild(MatSort, { static: true }) matSort: MatSort;

  @Input() tableColumns: TableColumn[];
  @Input() title: string = '';

  @Output() sort: EventEmitter<Sort> = new EventEmitter();

  @Output() procesoAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() subirAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() verAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() descargarAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() checkItemAction: EventEmitter<any> = new EventEmitter<any>();
  acciones = true;
  paginationSizes: number[] = [5, 10, 15];
  defaultPageSize = this.paginationSizes[1];

  tableDataSource = new MatTableDataSource([]);
  displayedColumns: string[];
  fontSize = 'fs-13';
  checked = false;

  const = Const;

  @Input() set tableData(data: any[]) {
    this.setTableDataSource(data);
  }


  constructor() { }

  ngOnInit(): void {
    const columnNames = this.tableColumns.map(
      (tableColumn: TableColumn) => tableColumn.name
    );
    this.displayedColumns = [
      ...columnNames,
      "Acciones",
    ];
  }

  ngAfterViewInit(): void {
    this.tableDataSource.sort = this.matSort;
    this.tableDataSource.paginator = this.matPaginator;
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

  emitCheckItem(row: any, checkvalue: any) {
    row.flagSeleccion = checkvalue.currentTarget.checked;
    this.checkItemAction.emit(row);
  }

  sortTable(sortParameters: Sort) {
    if (sortParameters.active !== 'estado') {
      sortParameters.active = this.tableColumns.find(
        (column) => column.name === sortParameters.active
      ).dataKey;
      this.sort.emit(sortParameters);
    } else {
      this.sort.emit(sortParameters);
    }
  }

}
