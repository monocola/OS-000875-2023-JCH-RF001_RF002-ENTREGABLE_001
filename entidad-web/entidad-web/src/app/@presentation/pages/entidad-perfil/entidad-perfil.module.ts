import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { EntidadPerfilRoutingModule } from './entidad-perfil-routing.module';
import { RouterModule } from '@angular/router';
import {
  NbAutocompleteModule,
  NbButtonModule,
  NbDatepickerModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbLayoutModule,
  NbMenuModule,
  NbPopoverModule,
  NbSelectModule,
  NbSidebarModule,
  NbTooltipModule,
} from '@nebular/theme';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { PagesRoutingModule } from '../pages-routing.module';
import { MatDividerModule } from '@angular/material/divider';
import { ThemeModule } from '../../@theme/theme.module';
import { CommonComponentsModule } from '../../@common-components/common-components.module';
import { EntidadPerfilComponent } from './entidad-perfil.component';
import { MatIconModule } from '@angular/material/icon';

@NgModule({
  declarations: [EntidadPerfilComponent],
  imports: [
    CommonModule,
    EntidadPerfilRoutingModule,
    RouterModule,
    ThemeModule,
    NbMenuModule,
    NbButtonModule,
    CommonComponentsModule,
    FormsModule,
    NbSelectModule,
    NbDatepickerModule,
    NbAutocompleteModule,
    NbTooltipModule,
    PagesRoutingModule,
    RouterModule,
    NbLayoutModule,
    NbSidebarModule,
    NbIconModule,
    NbFormFieldModule,
    ReactiveFormsModule,
    NbInputModule,
    NbPopoverModule,
    MatDividerModule,
    MatIconModule
  ]
})
export class EntidadPerfilModule {}
