import {
  AfterViewInit,
  Component,
  EventEmitter,
  Input,
  OnInit,
  Output,
  ViewChild,
} from '@angular/core';
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
import { HelperLeyComponentsPerfilesService } from '../helperComponentPerfiles.service';

@Component({
  selector: 'serv-talento-table-formacion-academica',
  templateUrl: './table-formacion-academica.component.html',
  styleUrls: ['./table-formacion-academica.component.scss'],
})
export class TableFormacionAcademicaComponent implements OnInit, AfterViewInit {
  @ViewChild(MatSort, { static: true }) matSort: MatSort;

  tableDataSource = new MatTableDataSource([]);
  displayedColumns: string[];
  const = Const;

  @Input() title: string = '';
  @Input() tamanio: number = 0;
  @Input() tableColumns: TableColumn[] = [];
  @Input() dataExport = new ExportExcelModel();
  @Input() showDownloadButton = false;

  @Output() sort: EventEmitter<Sort> = new EventEmitter();
  @Output() openDetalleConvocatoria = new EventEmitter();
  @Output() editAction: EventEmitter<any> = new EventEmitter<any>();
  @Output() removeAction: EventEmitter<any> = new EventEmitter<any>();

  @Input() set tableData(data: any[]) {
    this.setTableDataSource(data);
  }

  constructor(private exportExcelService: ExportExcelService,
    private helperService: HelperLeyComponentsPerfilesService) {}

  ngOnInit(): void {
    this.helperService.loadCombox ();

    const columnNames = this.tableColumns.map(
      (tableColumn: TableColumn) => tableColumn.name
    );
    this.displayedColumns = [
      ...columnNames,
      'nivel',
      'grado',
      'situacion',
      'carreras',
      'Acciones'
    ];
  }

  getNivelName (maeDetalleId: any) {
    for (let index = 0; index < this.helperService.nivelesEducativos.length; index++) {
      if (this.helperService.nivelesEducativos [index].maeDetalleId === maeDetalleId) {
        return this.helperService.nivelesEducativos [index].descripcion;
      }
    }

    return '-';
  }

  getGradoName (maeDetalleId: any) {
    for (let index = 0; index < this.helperService.grados.length; index++) {
      if (this.helperService.grados [index].maeDetalleId === maeDetalleId) {
        return this.helperService.grados [index].descripcion;
      }
    }

    return '-';
  }

  getSituacionName (maeDetalleId: any) {
    for (let index = 0; index < this.helperService.estadosGrado.length; index++) {
      if (this.helperService.estadosGrado [index].maeDetalleId === maeDetalleId) {
        return this.helperService.estadosGrado [index].descripcion;
      }
    }

    return '-';
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

  openDetalle(row) {
    this.openDetalleConvocatoria.emit(row);
  }

  capitalizeFirstLetter(string: string) {
    return string.charAt(0).toUpperCase() + string.slice(1).toLowerCase ();
  }

  emitEditAction (row: any) {
    this.editAction.emit(row);
  }

  emitDeleteElement (row: any) {
    this.removeAction.emit(row);
  }
}
