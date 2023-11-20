import { ReactiveFormsModule } from '@angular/forms';
import { MatAutocompleteModule } from '@angular/material/autocomplete';
import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { AppLoaderDirective } from './app-loader/app-loader.directive';
import { AppLoaderComponent } from './app-loader/app-loader.component';
import {
  NbButtonModule,
  NbCardModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbSelectModule,
  NbThemeModule,
} from '@nebular/theme';
import { TooltipInfoComponent } from './tooltip-info/tooltip-info.component';
import { FileVisualizerComponent } from './file-visualizer/file-visualizer.component';
import { MatCardModule } from '@angular/material/card';
import { MaterialTableComponent } from './material-table/material-table.component';
import { MatTableModule } from '@angular/material/table';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import {
  MatPaginatorIntl,
  MatPaginatorModule,
} from '@angular/material/paginator';
import { MatSortModule } from '@angular/material/sort';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonModule } from '@angular/material/button';
import { MatRippleModule } from '@angular/material/core';
import { ColorPipeGetterPipe } from './material-table/color-pipe-getter.pipe';
import { DataPropertyGetterPipe } from './material-table/data-property-getter.pipe';
import { getDutchPaginatorIntl } from './material-table/paginator-translate';
import { PaginatorComponent } from './paginator/paginator.component';
import { AutocompleteComponent } from './autocomplete/autocomplete.component';
import { UbigeoFormComponent } from './ubigeo-form/ubigeo-form.component';
import { ModalConfirmationComponent } from './modal-confirmation/modal-confirmation.component';
import { AddItemButtonComponent } from './add-item-button/add-item-button.component';
import { InputFieldComponent } from './input-field/input-field.component';
import { SelectFieldComponent } from './select-field/select-field.component';
import { TextareaComponent } from './textarea/textarea.component';
import { NgxExtendedPdfViewerModule } from 'ngx-extended-pdf-viewer';
import { InputFieldDisabledComponent } from './input-field-disabled/input-field-disabled.component';
import { SelectFieldDisabledComponent } from './select-field-disabled/select-field-disabled.component';
import { MaterialTableCheckboxComponent } from './material-table-checkbox/material-table-checkbox.component';
import { TablaContratosComponent } from './material-table-contratos/tabla-contratos.component';
import { CabeceraExamenComponent } from './cabecera-examen/cabecera-examen.component';
import { MatDividerModule } from '@angular/material/divider';
import { SimpleCharacteresAndNumberDirective } from './directivas/simple-characteres-and-number.directive';
import { NumberDirective } from './directivas/numbers-only.directive';
import { NumberPositiveOnlyDirective } from './directivas/numbers-positive-only.directive';

const COMPONENTS = [
  AppLoaderComponent,
  TooltipInfoComponent,
  FileVisualizerComponent,
  MaterialTableComponent,
  PaginatorComponent,
  AutocompleteComponent,
  UbigeoFormComponent,
  SelectFieldComponent,
  ModalConfirmationComponent,
  AddItemButtonComponent,
  InputFieldComponent,
  TextareaComponent,
  InputFieldDisabledComponent,
  SelectFieldDisabledComponent,
  MaterialTableCheckboxComponent,
  TablaContratosComponent,
  CabeceraExamenComponent
];
const DIRECTIVES = [
  AppLoaderDirective,
  ColorPipeGetterPipe,
  DataPropertyGetterPipe,
  SimpleCharacteresAndNumberDirective,
  NumberDirective,
  NumberPositiveOnlyDirective
];
@NgModule({
  declarations: [...COMPONENTS, ...DIRECTIVES],
  imports: [
    MatDividerModule,
    CommonModule,
    NbCardModule,
    NbIconModule,
    NbButtonModule,
    MatCardModule,
    MatTableModule,
    MatFormFieldModule,
    MatInputModule,
    MatPaginatorModule,
    MatSortModule,
    MatIconModule,
    MatButtonModule,
    NbFormFieldModule,
    NbInputModule,
    NbThemeModule,
    MatRippleModule,
    ReactiveFormsModule,
    MatAutocompleteModule,
    NbSelectModule,
    NgxExtendedPdfViewerModule,
  ],
  exports: [...COMPONENTS, ...DIRECTIVES],
  entryComponents: [...COMPONENTS],
  providers: [{ provide: MatPaginatorIntl, useValue: getDutchPaginatorIntl() }],
})
export class CommonComponentsModule {}
