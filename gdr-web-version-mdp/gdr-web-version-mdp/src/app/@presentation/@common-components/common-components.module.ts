import { ReactiveFormsModule } from '@angular/forms';
import { MatAutocompleteModule } from '@angular/material/autocomplete';
import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { AppLoaderDirective } from './app-loader/app-loader.directive';
import { AppLoaderComponent } from './app-loader/app-loader.component';
import {
  NbButtonModule,
  NbCardModule, NbCheckboxModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbSelectModule,
  NbThemeModule,
  NbActionsModule,
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
import { MaterialTableCheckSelectComponent } from './material-table-check-select/material-table-check-select.component';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { ColorPipeGetterPipe2 } from './material-table-check-select/color-pipe-getter.pipe';
import { DataPropertyGetterPipe2 } from './material-table-check-select/data-property-getter.pipe';
import { ModalEliminarComponent } from './modal-eliminar/modal-eliminar.component';
import { MatDividerModule } from '@angular/material/divider';
import { PersonalCardComponent } from './personal-card/personal-card.component';
import { BackButtonComponent } from './back-button/back-button.component';
import { SimplePagerComponent } from './simple-pager/simple-pager.component';
import { FontAwesomeModule } from '@fortawesome/angular-fontawesome';
import { OnlyNumberDirective } from './directivas/only-number.directive';
import { AlertabackComponent } from './alertaback/alertaback.component';
import { TwoDecinalDirective } from './directivas/two-decinal.directive';
import { NgDropFilesDirective } from './directivas/ng-drop-files.directive';
import { FileDropzoneComponent } from './file-dropzone/file-dropzone.component';
import { OnlyWriteDirective } from './directivas/only-write.directive';
import { PersonalCardDetalleComponent } from './personal-card -detalle/personal-card-detalle.component';
import { SimplePagerSliderComponent } from './simple-pager-slider/simple-pager-slider.component';

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
  MaterialTableCheckSelectComponent,
  PersonalCardComponent,
  PersonalCardDetalleComponent,

  ModalEliminarComponent,
  BackButtonComponent,
  FileDropzoneComponent
];
const DIRECTIVES = [
  AppLoaderDirective,
  ColorPipeGetterPipe,
  DataPropertyGetterPipe,
  ColorPipeGetterPipe2,
  DataPropertyGetterPipe2,
  SimplePagerComponent,
  SimplePagerSliderComponent,
  OnlyNumberDirective,
  NgDropFilesDirective
];
@NgModule({
  declarations: [...COMPONENTS, ...DIRECTIVES, AlertabackComponent, TwoDecinalDirective, NgDropFilesDirective, OnlyWriteDirective],
  imports: [
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
    MatCheckboxModule,
    MatDividerModule,
    FontAwesomeModule,
    NbActionsModule,
    NbCheckboxModule
  ],
  exports: [...COMPONENTS, ...DIRECTIVES, TwoDecinalDirective, OnlyWriteDirective],
  entryComponents: [...COMPONENTS],
  providers: [{ provide: MatPaginatorIntl, useValue: getDutchPaginatorIntl() }],
})
export class CommonComponentsModule {}
