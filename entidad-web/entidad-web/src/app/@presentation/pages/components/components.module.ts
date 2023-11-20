import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { HeaderComponent } from './header/header.component';
import { SidenavComponent } from './sidenav/sidenav.component';
import { RouterModule } from '@angular/router';

import {
  NbButtonModule,
  NbCardModule,
  NbFormFieldModule,
  NbInputModule,
  NbToggleModule,
  NbIconModule,
  NbLayoutModule,
  NbMenuModule,
  NbSidebarModule,
} from '@nebular/theme';

import { MatDividerModule } from '@angular/material/divider';
import { MatMenuModule } from '@angular/material/menu';
import { FooterComponent } from './footer/footer.component';
import { FontAwesomeModule } from '@fortawesome/angular-fontawesome';
import { ModalCambioContraComponent } from './modal-cambio-contra/modal-cambio-contra.component';
import { ReactiveFormsModule } from '@angular/forms';

@NgModule({
  declarations: [HeaderComponent, SidenavComponent, FooterComponent, ModalCambioContraComponent],
  imports: [
    CommonModule,
    MatMenuModule,
    RouterModule,
    NbIconModule,
    NbLayoutModule,
    NbMenuModule,
    NbSidebarModule,
    MatDividerModule,
    FontAwesomeModule,
    NbButtonModule,
    NbFormFieldModule,
    NbCardModule,
    NbToggleModule,
    NbInputModule,
    ReactiveFormsModule
  ],
  exports: [HeaderComponent, SidenavComponent, FooterComponent],
})
export class PagesComponentsModule {}
