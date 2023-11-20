import { CUSTOM_ELEMENTS_SCHEMA, NgModule } from '@angular/core';
import { CommonModule, CurrencyPipe } from '@angular/common';
import { ListaEvaluacionesRoutingModule } from './lista-evaluaciones-routing.module';
import { DialogRegistroNotasEvalComponent } from './lista-gestion-notas/dialogo-registro-notas-eval/dialog-registro-notas-eval.component';
import { ListaEvaluacionesComponent } from './lista-evaluaciones.component';
import { ListaGestionNotasComponent } from './lista-gestion-notas/lista-gestion-notas.component';
import { RegistroNotasIndividialComponent } from './lista-gestion-notas/dialogo-registro-notas-eval/registro-notas-individual/registro-notas-individial.component';
import { ResumenEvaluacionesComponent } from './resumen-evaluaciones/resumen-evaluaciones.component';
import { RegistroNotasMasivoComponent } from './lista-gestion-notas/dialogo-registro-notas-eval/registro-notas-masivo/registro-notas-masivo.component';

import {
  NbAccordionModule, NbActionsModule,
  NbAutocompleteModule,
  NbButtonModule,
  NbCardModule,
  NbCheckboxModule,
  NbDatepickerModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbPopoverModule,
  NbRadioModule,
  NbSelectModule
} from '@nebular/theme';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatDividerModule } from '@angular/material/divider';
import { MatRippleModule } from '@angular/material/core';
import { CommonComponentsModule } from '../../@common-components/common-components.module';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import {
  NgxMatDatetimePickerModule,
  NgxMatNativeDateModule,
  NgxMatTimepickerModule,
} from '@angular-material-components/datetime-picker';
import { MatButtonModule } from '@angular/material/button';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MatRadioModule } from '@angular/material/radio';
import { MatSelectModule } from '@angular/material/select';
import { MatCardModule } from '@angular/material/card';
import { MatDialogModule } from '@angular/material/dialog';
import { MatTabsModule } from '@angular/material/tabs';
import { MatListModule } from '@angular/material/list';
import { MatCarouselModule } from '@ngmodule/material-carousel';
import { RegistroOtrasNotasIndividualComponent } from './lista-gestion-notas/dialogo-registro-otras-notas/registro-otras-notas-individual/registro-otras-notas-individual.component';
import { DialogoRegistroOtrasNotasComponent } from './lista-gestion-notas/dialogo-registro-otras-notas/dialogo-registro-otras-notas.component';
import { RegistroOtrasNotasMasivoComponent } from './lista-gestion-notas/dialogo-registro-otras-notas/registro-otras-notas-masivo/registro-otras-notas-masivo.component';
import { StrictNumberOnlyDirectiveModule } from '../../@common-components/onlyNumber/StrictNumberOnlyDirective';
import { TablaGestionNotasComponent } from './lista-gestion-notas/tabla-gestion-notas/tabla-gestion-notas.component';
import { MatPaginatorModule } from '@angular/material/paginator';
import { MatTableModule } from '@angular/material/table';
import { MatSortModule } from '@angular/material/sort';

@NgModule({
  declarations: [
    ListaEvaluacionesComponent,
    ListaGestionNotasComponent,
    ResumenEvaluacionesComponent,
    DialogRegistroNotasEvalComponent,
    DialogoRegistroOtrasNotasComponent,
    RegistroNotasIndividialComponent,
    RegistroNotasMasivoComponent,
    RegistroOtrasNotasIndividualComponent,
    RegistroOtrasNotasMasivoComponent,
    TablaGestionNotasComponent,
  ],
  imports: [
    CommonModule,
    ListaEvaluacionesRoutingModule,
    MatDividerModule,
    MatRippleModule,
    NbButtonModule,
    NbAccordionModule,
    CommonComponentsModule,
    MatFormFieldModule,
    MatInputModule,
    FormsModule,
    ReactiveFormsModule,
    NbIconModule,
    NbInputModule,
    NbCardModule,
    NbDatepickerModule,
    NgxMatDatetimePickerModule,
    NgxMatTimepickerModule,
    NgxMatNativeDateModule,
    MatButtonModule,
    MatCheckboxModule,
    MatDatepickerModule,
    MatRadioModule,
    MatSelectModule,
    MatCardModule,
    NbFormFieldModule,
    NbPopoverModule,
    NbSelectModule,
    NbAutocompleteModule,
    MatDialogModule,
    MatTabsModule,
    MatListModule,
    MatCarouselModule,
    NbRadioModule,
    StrictNumberOnlyDirectiveModule,
    NbCheckboxModule,
    NbActionsModule,
    MatPaginatorModule,
    MatTableModule,
    MatSortModule
  ],
  providers: [CurrencyPipe],
  schemas: [CUSTOM_ELEMENTS_SCHEMA],
})
export class ListaEvaluacionesModule {}
