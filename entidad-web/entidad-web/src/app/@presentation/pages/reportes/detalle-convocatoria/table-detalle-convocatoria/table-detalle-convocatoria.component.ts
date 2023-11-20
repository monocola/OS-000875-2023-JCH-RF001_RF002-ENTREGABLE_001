import { AfterViewInit, Component, EventEmitter, Input, OnInit, Output, ViewChild,} from '@angular/core';
import { MatSort, Sort } from '@angular/material/sort';
import { TableColumn } from '../../../../@common-components/material-table/table-column';
import { MatTableDataSource } from '@angular/material/table';
import { Const } from '../../../../../@data/services/const';
import { from } from 'rxjs';
import { map, toArray } from 'rxjs/operators';

@Component({
  selector: 'serv-talento-table-detalle-convocatoria',
  templateUrl: './table-detalle-convocatoria.component.html',
  styleUrls: ['./table-detalle-convocatoria.component.scss']
})
export class TableDetalleConvocatoriaComponent implements OnInit, AfterViewInit {
  @ViewChild(MatSort, { static: true }) matSort: MatSort;

  @Input() title: string = '';
  @Output() sort: EventEmitter<Sort> = new EventEmitter();
  tableDataSource = new MatTableDataSource([]);
  displayedColumns: string[];
  @Output() pageEvent: EventEmitter<any> = new EventEmitter<any>();

  const = Const;

  @Input() set tableData(data: any[]) {
    this.setTableDataSource(data);
  }

  constructor() {}

  ngOnInit(): void {

    this.displayedColumns = [ 'PERFIL','DEPARTAMENTO','REMUNERACION','VAC','POST','CALIF'];
  }

  ngAfterViewInit(): void {
    this.tableDataSource.sort = this.matSort;
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
    this.sort.emit(sortParameters);
  }

}
