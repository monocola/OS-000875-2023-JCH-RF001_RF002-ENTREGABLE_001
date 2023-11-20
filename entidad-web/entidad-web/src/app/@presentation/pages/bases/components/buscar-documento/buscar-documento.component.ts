import {
  ChangeDetectorRef,
  Component,
  Input,
  OnChanges,
  OnInit,
  Output,
} from '@angular/core';
import { FormControl } from '@angular/forms';
import { EventEmitter } from '@angular/core';

@Component({
  selector: 'buscar-documento',
  templateUrl: './buscar-documento.component.html',
  styleUrls: ['./buscar-documento.component.scss'],
})
export class BuscarDocumentoComponent implements OnInit, OnChanges {
  @Input() controlDocument: FormControl = null;
  @Input() items: any[] = [];
  @Input() fieldToShow = 'default';
  @Input() maxItems = 1;
  @Input() itemsSeleccionados: any[] = [];
  @Input() label = 'DEFAULT_LABEL';
  @Input() placeholder = 'DEFAULT_PLACEHOLDER';

  @Output() selectedChange = new EventEmitter<any>();
  @Output() pdfSelected = new EventEmitter<any>();
  
  itemsFiltrados: any[] = [];
  informesSeleccionadosMap: Map <any, any> = new Map<any, any> ();
  field = '';
  control: FormControl = new FormControl({ value: '', disabled: true });

  constructor(private cdr: ChangeDetectorRef) {}

  ngOnInit(): void {
    this.control.enable();
  }

  ngOnChanges() {
    this.verifyElements();
    this.cdr.detectChanges();
  }

  addNewDocument(elementSelected) {
    this.controlDocument.markAsDirty();
    if (elementSelected && this.control) {
      if (!this.informesSeleccionadosMap.has (elementSelected.value)) {
        this.informesSeleccionadosMap.set (elementSelected.value, true);
        this.items = this.items.filter((el) => el !== elementSelected);
        this.itemsSeleccionados = [...this.itemsSeleccionados, elementSelected];
        this.selectedChange.emit(this.itemsSeleccionados);
        this.control.patchValue('');
        this.verifyElements();
      }
    }
  }

  verifyElements() {
    if (this.itemsSeleccionados.length >= this.maxItems) {
      this.control.disable();
    } else {
      this.control.enable();
    }
    this.cdr.detectChanges();

    this.items = this.items.filter ((item: any) => {
      return !this.informesSeleccionadosMap.has (item.value);
    });
  }

  remove(item) {
    if (
      this.items.filter((i: { value: any }) => i.value === item.value)
        .length === 0
    ) {
      this.items.push(item);
    }

    this.items.sort((a, b) =>
      a.description > b.description ? 1 : b.description > a.description ? -1 : 0
    );
    this.itemsSeleccionados = this.itemsSeleccionados.filter(
      (itemfilter: { value: any }) => itemfilter.value !== item.value
    );
    this.informesSeleccionadosMap.delete (item.value);
    this.controlDocument.patchValue([...this.itemsSeleccionados]);
    this.verifyElements();
  }

  view(item) {
    this.pdfSelected.emit(item);
  }
}
