import {
  Component,
  Input,
  OnChanges,
  Output,
  SimpleChanges,
  EventEmitter
} from '@angular/core';
import { AbstractControl, FormControl } from '@angular/forms';
import { map, startWith, tap } from 'rxjs/operators';
import { Subscription } from 'rxjs';

@Component({
  selector: 'autocomplete',
  templateUrl: './autocomplete.component.html',
})
export class AutocompleteComponent implements OnChanges {
  @Input() control: FormControl = null;
  @Input() items: any[] = [];
  @Input() fieldToShow = 'default';
  @Input() placeholder = 'Placeholder';
  @Input() label = '';
  @Input() requiredField = true;
  @Input() invalidFlag = false;

  @Input() size = 'large';

  @Output() selectedChange = new EventEmitter();
  @Output() keyup = new EventEmitter();

  itemsFiltrados: any = [];
  field = '';

  subscription: Subscription = null;

  ngOnChanges(changes: SimpleChanges): void {
    if (changes.items?.currentValue?.length > 0 || changes.control) {
      this.initializeSubscription();
    }
  }

  displayFn(item: any): string {
    if (item) {
      return item[this.fieldToShow] ? item[this.fieldToShow] : '';
    }
  }

  initializeSubscription() {
    this.subscription = this.control.valueChanges
    .pipe(
      startWith(''),
      map((value) => this._filterItems(value)),
      tap(() => this._validValue(this.control))
    )
    .subscribe();
  }

  private _validValue(control: AbstractControl) {
    const typeValue = typeof control.value;
    if (typeValue === 'string' && this.requiredField) {
      control.setErrors({ notfound: true });
    }
    if (control.value === '') {
      control.reset();
    }
    if (typeValue === 'object') {
      this.selectedChange.emit(this.control.value);
    }
  }

  _filterItems(value) {
    if (typeof value === 'string' && value.length > 0) {
        const query = value.toUpperCase();
        this.itemsFiltrados = this.items.filter((items) =>
          items[this.fieldToShow].toUpperCase().includes(query)
        );
    } else {
      this.itemsFiltrados = [];
    }
  }
}
