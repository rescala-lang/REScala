import { step, TestSettings, By, Until, beforeAll, afterAll, Key } from '@flood/element'

const path = require('path')

export const settings: TestSettings = {
	loopCount: 1,
	description: "Add 300 Todos in TodoApp",
	screenShotOnFailure: true,
	actionDelay: 0.01,
	stepDelay: 0.1,

	// Automatically wait for elements before trying to interact with them
	waitUntil: 'visible',
}

export default () => {
	beforeAll(async browser => {
		await browser.wait('500ms')
	})

	afterAll(async browser => {
		await browser.wait('500ms')
	})

	step('Start', async browser => {
		await browser.visit(path.resolve('../Examples/Todolist/index.html'))
	})

	for(let i = 0; i < 300; i++) {
		step(`Add ${i}`, async browser => {
		await browser.type(By.id('newtodo'), i.toString())
		await browser.press(Key.ENTER)
		await browser.wait(Until.elementIsVisible(By.visibleText(i.toString())))
		i++
	})
	}
}