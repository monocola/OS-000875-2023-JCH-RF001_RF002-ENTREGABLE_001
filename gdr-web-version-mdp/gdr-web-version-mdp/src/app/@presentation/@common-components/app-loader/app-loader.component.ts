import { Component, HostBinding, Input } from '@angular/core';
import { NbComponentSize, NbComponentStatus } from "@nebular/theme";

@Component({
  // tslint:disable-next-line: component-selector
  selector: 'app-loader',
  template: `
    <div class="wrapper">
      <div class="loader--square"></div>
    </div>
  `,
  styleUrls: ['./app-loader.component.less'],
})
export class AppLoaderComponent {

  /**
   * Loading text that is shown near the icon
   * @type string
   */
  @Input()
  message: string = 'Estamos cargando data...';

  /**
   * Spinner size, available sizes:
   * tiny, small, medium, large, giant
   * @param {string} value
   */
  @Input()
  size: NbComponentSize = 'medium';

  /**
   * Spinner status (adds specific styles):
   * `basic`, `primary`, `info`, `success`, `warning`, `danger`, `control`.
   */
  @Input()
  get status(): NbComponentStatus {
    return this._status;
  }
  set status(value: NbComponentStatus) {
    if ((value as string) === '') {
      this.emptyStatusWarning('AppLoader');
      value = 'primary';
    }
    this._status = value;
  }
  protected _status: NbComponentStatus = 'primary';

  @HostBinding('class.size-tiny')
  get tiny() {
    return this.size === 'tiny';
  }

  @HostBinding('class.size-small')
  get small() {
    return this.size === 'small';
  }

  @HostBinding('class.size-medium')
  get medium() {
    return this.size === 'medium';
  }

  @HostBinding('class.size-large')
  get large() {
    return this.size === 'large';
  }

  @HostBinding('class.size-giant')
  get giant() {
    return this.size === 'giant';
  }

  @HostBinding('class.status-primary')
  get primary() {
    return this.status === 'primary';
  }

  @HostBinding('class.status-info')
  get info() {
    return this.status === 'info';
  }

  @HostBinding('class.status-success')
  get success() {
    return this.status === 'success';
  }

  @HostBinding('class.status-warning')
  get warning() {
    return this.status === 'warning';
  }

  @HostBinding('class.status-danger')
  get danger() {
    return this.status === 'danger';
  }
  emptyStatusWarning(source: string) {
    console.warn(`${source}: Using empty string as a status is deprecated. Use \`basic\` instead.`);
  }
}
